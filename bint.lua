local function luainteger_bitsize()
  local n, i = -1, 0
  repeat
    n, i = n >> 16, i + 16
  until n==0
  return i
end

local math_type = math.type
local math_floor = math.floor
local math_abs = math.abs
local math_ceil = math.ceil
local math_modf = math.modf
local math_mininteger = math.mininteger
local math_maxinteger = math.maxinteger
local math_max = math.max
local math_min = math.min
local string_format = string.format
local table_insert = table.insert
local table_concat = table.concat
local table_unpack = table.unpack

local memo = {}

local function newmodule(bits, wordbits)

local intbits = luainteger_bitsize()
bits = bits or 256
wordbits = wordbits or (intbits // 2)

local memoindex = bits * 64 + wordbits
if memo[memoindex] then
  return memo[memoindex]
end

assert(bits % wordbits == 0, 'bitsize is not multiple of word bitsize')
assert(2*wordbits <= intbits, 'word bitsize must be half of the lua integer bitsize')
assert(bits >= 64, 'bitsize must be >= 64')
assert(wordbits >= 8, 'wordbits must be at least 8')
assert(bits % 8 == 0, 'bitsize must be multiple of 8')

local bint = {}
bint.__index = bint

bint.bits = bits

local BINT_BITS = bits
local BINT_BYTES = bits // 8
local BINT_WORDBITS = wordbits
local BINT_SIZE = BINT_BITS // BINT_WORDBITS
local BINT_WORDMAX = (1 << BINT_WORDBITS) - 1
local BINT_WORDMSB = (1 << (BINT_WORDBITS - 1))
local BINT_LEPACKFMT = '<'..('I'..(wordbits // 8)):rep(BINT_SIZE)
local BINT_MATHMININTEGER, BINT_MATHMAXINTEGER
local BINT_MININTEGER

function bint.zero()
  local x = setmetatable({}, bint)
  for i=1,BINT_SIZE do
    x[i] = 0
  end
  return x
end
local bint_zero = bint.zero

function bint.one()
  local x = setmetatable({}, bint)
  x[1] = 1
  for i=2,BINT_SIZE do
    x[i] = 0
  end
  return x
end
local bint_one = bint.one

local function tointeger(x)
  x = tonumber(x)
  local ty = math_type(x)
  if ty == 'float' then
    local floorx = math_floor(x)
    if floorx == x then
      x = floorx
      ty = math_type(x)
    end
  end
  if ty == 'integer' then
    return x
  end
end

function bint.fromuinteger(x)
  x = tointeger(x)
  if x then
    if x == 1 then
      return bint_one()
    elseif x == 0 then
      return bint_zero()
    end
    local n = setmetatable({}, bint)
    for i=1,BINT_SIZE do
      n[i] = x & BINT_WORDMAX
      x = x >> BINT_WORDBITS
    end
    return n
  end
end
local bint_fromuinteger = bint.fromuinteger

function bint.frominteger(x)
  x = tointeger(x)
  if x then
    if x == 1 then
      return bint_one()
    elseif x == 0 then
      return bint_zero()
    end
    local neg = false
    if x < 0 then
      x = math_abs(x)
      neg = true
    end
    local n = setmetatable({}, bint)
    for i=1,BINT_SIZE do
      n[i] = x & BINT_WORDMAX
      x = x >> BINT_WORDBITS
    end
    if neg then
      n:_unm()
    end
    return n
  end
end
local bint_frominteger = bint.frominteger

local basesteps = {}

local function getbasestep(base)
  local step = basesteps[base]
  if step then
    return step
  end
  step = 0
  local dmax = 1
  local limit = math_maxinteger // base
  repeat
    step = step + 1
    dmax = dmax * base
  until dmax >= limit
  basesteps[base] = step
  return step
end

local function ipow(y, x, n)
  if n == 1 then
    return y * x
  elseif n & 1 == 0 then
    return ipow(y, x * x, n // 2)
  end
  return ipow(x * y, x * x, (n-1) // 2)
end

function bint.frombase(s, base)
  if type(s) ~= 'string' then
    return
  end
  base = base or 10
  if not (base >= 2 and base <= 36) then
    return
  end
  local step = getbasestep(base)
  if #s < step then
    return bint_frominteger(tonumber(s, base))
  end
  local sign, int = s:lower():match('^([+-]?)(%w+)$')
  if not (sign and int) then
    return
  end
  local n = bint_zero()
  for i=1,#int,step do
    local part = int:sub(i,i+step-1)
    local d = tonumber(part, base)
    if not d then
      return
    end
    if i > 1 then
      n = n * ipow(1, base, #part)
    end
    if d ~= 0 then
      n:_add(d)
    end
  end
  if sign == '-' then
    n:_unm()
  end
  return n
end
local bint_frombase = bint.frombase

function bint.fromstring(s)
  if type(s) ~= 'string' then
    return
  end
  if s:find('^[+-]?[0-9]+$') then
    return bint_frombase(s, 10)
  elseif s:find('^[+-]?0[xX][0-9a-fA-F]+$') then
    return bint_frombase(s:gsub('0[xX]', '', 1), 16)
  elseif s:find('^[+-]?0[bB][01]+$') then
    return bint_frombase(s:gsub('0[bB]', '', 1), 2)
  end
end
local bint_fromstring = bint.fromstring

function bint.fromle(buffer)
  assert(type(buffer) == 'string', 'buffer is not a string')
  if #buffer > BINT_BYTES then
    buffer = buffer:sub(1, BINT_BYTES)
  elseif #buffer < BINT_BYTES then
    buffer = buffer..('\x00'):rep(BINT_BYTES - #buffer)
  end
  return setmetatable({BINT_LEPACKFMT:unpack(buffer)}, bint)
end

function bint.frombe(buffer)
  assert(type(buffer) == 'string', 'buffer is not a string')
  if #buffer > BINT_BYTES then
    buffer = buffer:sub(-BINT_BYTES, #buffer)
  elseif #buffer < BINT_BYTES then
    buffer = ('\x00'):rep(BINT_BYTES - #buffer)..buffer
  end
  return setmetatable({BINT_LEPACKFMT:unpack(buffer:reverse())}, bint)
end

function bint.new(x)
  if getmetatable(x) ~= bint then
    local ty = type(x)
    if ty == 'number' then
      x = bint_frominteger(x)
    elseif ty == 'string' then
      x = bint_fromstring(x)
    end
    assert(x, 'value cannot be represented by a bint')
    return x
  end
  local n = setmetatable({}, bint)
  for i=1,BINT_SIZE do
    n[i] = x[i]
  end
  return n
end
local bint_new = bint.new

function bint.tobint(x, clone)
  if getmetatable(x) == bint then
    if not clone then
      return x
    end
    local n = setmetatable({}, bint)
    for i=1,BINT_SIZE do
      n[i] = x[i]
    end
    return n
  end
  local ty = type(x)
  if ty == 'number' then
    return bint_frominteger(x)
  elseif ty == 'string' then
    return bint_fromstring(x)
  end
end
local tobint = bint.tobint

function bint.parse(x, clone)
  local i = tobint(x, clone)
  if i then
    return i
  end
  return tonumber(x)
end
local bint_parse = bint.parse

function bint.touinteger(x)
  if getmetatable(x) == bint then
    local n = 0
    for i=1,BINT_SIZE do
      n = n | (x[i] << (BINT_WORDBITS * (i - 1)))
    end
    return n
  end
  return tointeger(x)
end

function bint.tointeger(x)
  if getmetatable(x) == bint then
    local n = 0
    local neg = x:isneg()
    if neg then
      x = -x
    end
    for i=1,BINT_SIZE do
      n = n | (x[i] << (BINT_WORDBITS * (i - 1)))
    end
    if neg then
      n = -n
    end
    return n
  end
  return tointeger(x)
end
local bint_tointeger = bint.tointeger

local function bint_assert_tointeger(x)
  x = bint_tointeger(x)
  if not x then
    error('value has no integer representation')
  end
  return x
end

function bint.tonumber(x)
  if getmetatable(x) == bint then
    if x <= BINT_MATHMAXINTEGER and x >= BINT_MATHMININTEGER then
      return x:tointeger()
    end
    return tonumber(tostring(x))
  end
  return tonumber(x)
end
local bint_tonumber = bint.tonumber

local BASE_LETTERS = {}
do
  for i=1,36 do
    BASE_LETTERS[i-1] = ('0123456789abcdefghijklmnopqrstuvwxyz'):sub(i,i)
  end
end

function bint.tobase(x, base, unsigned)
  x = tobint(x)
  if not x then
    return
  end
  base = base or 10
  if not (base >= 2 and base <= 36) then
    return
  end
  if unsigned == nil then
    unsigned = base ~= 10
  end
  local isxneg = x:isneg()
  if (base == 10 and not unsigned) or (base == 16 and unsigned and not isxneg) then
    if x <= BINT_MATHMAXINTEGER and x >= BINT_MATHMININTEGER then
      local n = x:tointeger()
      if base == 10 then
        return tostring(n)
      elseif unsigned then
        return string_format('%x', n)
      end
    end
  end
  local ss = {}
  local neg = not unsigned and isxneg
  x = neg and x:abs() or bint_new(x)
  local xiszero = x:iszero()
  if xiszero then
    return '0'
  end
  local step = 0
  local basepow = 1
  local limit = (BINT_WORDMSB - 1) // base
  repeat
    step = step + 1
    basepow = basepow * base
  until basepow >= limit
  local size = BINT_SIZE
  local xd, carry, d
  repeat
    carry = 0
    xiszero = true
    for i=size,1,-1 do
      carry = carry | x[i]
      d, xd = carry // basepow, carry % basepow
      if xiszero and d ~= 0 then
        size = i
        xiszero = false
      end
      x[i] = d
      carry = xd << BINT_WORDBITS
    end
    for _=1,step do
      xd, d = xd // base, xd % base
      if xiszero and xd == 0 and d == 0 then
        break
      end
      table_insert(ss, 1, BASE_LETTERS[d])
    end
  until xiszero
  if neg then
    table_insert(ss, 1, '-')
  end
  return table_concat(ss)
end

local function bint_assert_convert(x)
  return assert(tobint(x), 'value has not integer representation')
end

function bint.tole(x, trim)
  x = bint_assert_convert(x)
  local s = BINT_LEPACKFMT:pack(table_unpack(x))
  if trim then
    s = s:gsub('\x00+$', '')
    if s == '' then
      s = '\x00'
    end
  end
  return s
end

function bint.tobe(x, trim)
  x = bint_assert_convert(x)
  local s = BINT_LEPACKFMT:pack(table_unpack(x)):reverse()
  if trim then
    s = s:gsub('^\x00+', '')
    if s == '' then
      s = '\x00'
    end
  end
  return s
end

function bint.iszero(x)
  if getmetatable(x) == bint then
    for i=1,BINT_SIZE do
      if x[i] ~= 0 then
        return false
      end
    end
    return true
  end
  return x == 0
end

function bint.isone(x)
  if getmetatable(x) == bint then
    if x[1] ~= 1 then
      return false
    end
    for i=2,BINT_SIZE do
      if x[i] ~= 0 then
        return false
      end
    end
    return true
  end
  return x == 1
end

function bint.isminusone(x)
  if getmetatable(x) == bint then
    for i=1,BINT_SIZE do
      if x[i] ~= BINT_WORDMAX then
        return false
      end
    end
    return true
  end
  return x == -1
end
local bint_isminusone = bint.isminusone

function bint.isbint(x)
  return getmetatable(x) == bint
end

function bint.isintegral(x)
  return getmetatable(x) == bint or math_type(x) == 'integer'
end

function bint.isnumeric(x)
  return getmetatable(x) == bint or type(x) == 'number'
end

function bint.type(x)
  if getmetatable(x) == bint then
    return 'bint'
  end
  return math_type(x)
end

function bint.isneg(x)
  if getmetatable(x) == bint then
    return x[BINT_SIZE] & BINT_WORDMSB ~= 0
  end
  return x < 0
end
local bint_isneg = bint.isneg

function bint.ispos(x)
  if getmetatable(x) == bint then
    return not x:isneg() and not x:iszero()
  end
  return x > 0
end

function bint.iseven(x)
  if getmetatable(x) == bint then
    return x[1] & 1 == 0
  end
  return math_abs(x) % 2 == 0
end

function bint.isodd(x)
  if getmetatable(x) == bint then
    return x[1] & 1 == 1
  end
  return math_abs(x) % 2 == 1
end

function bint.maxinteger()
  local x = setmetatable({}, bint)
  for i=1,BINT_SIZE-1 do
    x[i] = BINT_WORDMAX
  end
  x[BINT_SIZE] = BINT_WORDMAX ~ BINT_WORDMSB
  return x
end

function bint.mininteger()
  local x = setmetatable({}, bint)
  for i=1,BINT_SIZE-1 do
    x[i] = 0
  end
  x[BINT_SIZE] = BINT_WORDMSB
  return x
end

function bint:_shlone()
  local wordbitsm1 = BINT_WORDBITS - 1
  for i=BINT_SIZE,2,-1 do
    self[i] = ((self[i] << 1) | (self[i-1] >> wordbitsm1)) & BINT_WORDMAX
  end
  self[1] = (self[1] << 1) & BINT_WORDMAX
  return self
end

function bint:_shrone()
  local wordbitsm1 = BINT_WORDBITS - 1
  for i=1,BINT_SIZE-1 do
    self[i] = ((self[i] >> 1) | (self[i+1] << wordbitsm1)) & BINT_WORDMAX
  end
  self[BINT_SIZE] = self[BINT_SIZE] >> 1
  return self
end

function bint:_shlwords(n)
  for i=BINT_SIZE,n+1,-1 do
    self[i] = self[i - n]
  end
  for i=1,n do
    self[i] = 0
  end
  return self
end

function bint:_shrwords(n)
  if n < BINT_SIZE then
    for i=1,BINT_SIZE-n do
      self[i] = self[i + n]
    end
    for i=BINT_SIZE-n+1,BINT_SIZE do
      self[i] = 0
    end
  else
    for i=1,BINT_SIZE do
      self[i] = 0
    end
  end
  return self
end

function bint:_inc()
  for i=1,BINT_SIZE do
    local tmp = self[i]
    local v = (tmp + 1) & BINT_WORDMAX
    self[i] = v
    if v > tmp then
      break
    end
  end
  return self
end

function bint.inc(x)
  local ix = tobint(x, true)
  if ix then
    return ix:_inc()
  end
  return x + 1
end

function bint:_dec()
  for i=1,BINT_SIZE do
    local tmp = self[i]
    local v = (tmp - 1) & BINT_WORDMAX
    self[i] = v
    if v <= tmp then
      break
    end
  end
  return self
end

function bint.dec(x)
  local ix = tobint(x, true)
  if ix then
    return ix:_dec()
  end
  return x - 1
end

function bint:_assign(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = y[i]
  end
  return self
end

function bint:_abs()
  if self:isneg() then
    self:_unm()
  end
  return self
end

function bint.abs(x)
  local ix = tobint(x, true)
  if ix then
    return ix:_abs()
  end
  return math_abs(x)
end
local bint_abs = bint.abs

function bint.floor(x)
  if getmetatable(x) == bint then
    return bint_new(x)
  end
  return bint_new(math_floor(tonumber(x)))
end

function bint.ceil(x)
  if getmetatable(x) == bint then
    return bint_new(x)
  end
  return bint_new(math_ceil(tonumber(x)))
end

function bint.bwrap(x, y)
  x = bint_assert_convert(x)
  if y <= 0 then
    return bint_zero()
  elseif y < BINT_BITS then
    return x & (bint_one() << y):_dec()
  end
  return bint_new(x)
end

function bint.brol(x, y)
  x, y = bint_assert_convert(x), bint_assert_tointeger(y)
  if y > 0 then
    return (x << y) | (x >> (BINT_BITS - y))
  elseif y < 0 then
    if y ~= math_mininteger then
      return x:bror(-y)
    else
      x:bror(-(y+1))
      x:bror(1)
    end
  end
  return x
end

function bint.bror(x, y)
  x, y = bint_assert_convert(x), bint_assert_tointeger(y)
  if y > 0 then
    return (x >> y) | (x << (BINT_BITS - y))
  elseif y < 0 then
    if y ~= math_mininteger then
      return x:brol(-y)
    else
      x:brol(-(y+1))
      x:brol(1)
    end
  end
  return x
end

function bint.trunc(x)
  if getmetatable(x) ~= bint then
    x = tonumber(x)
    if x then
      local ty = math_type(x)
      if ty == 'float' then
        x = math_modf(x)
      end
      return bint_frominteger(x)
    end
    return
  end
  return bint_new(x)
end

function bint.max(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    return bint_new(ix > iy and ix or iy)
  end
  return bint_parse(math_max(x, y))
end

function bint.min(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    return bint_new(ix < iy and ix or iy)
  end
  return bint_parse(math_min(x, y))
end

function bint:_add(y)
  y = bint_assert_convert(y)
  local carry = 0
  for i=1,BINT_SIZE do
    local tmp = self[i] + y[i] + carry
    carry = tmp >> BINT_WORDBITS
    self[i] = tmp & BINT_WORDMAX
  end
  return self
end

function bint.__add(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local z = setmetatable({}, bint)
    local carry = 0
    for i=1,BINT_SIZE do
      local tmp = ix[i] + iy[i] + carry
      carry = tmp >> BINT_WORDBITS
      z[i] = tmp & BINT_WORDMAX
    end
    return z
  end
  return bint_tonumber(x) + bint_tonumber(y)
end

function bint:_sub(y)
  y = bint_assert_convert(y)
  local borrow = 0
  local wordmaxp1 = BINT_WORDMAX + 1
  for i=1,BINT_SIZE do
    local res = self[i] + wordmaxp1 - y[i] - borrow
    self[i] = res & BINT_WORDMAX
    borrow = (res >> BINT_WORDBITS) ~ 1
  end
  return self
end

function bint.__sub(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local z = setmetatable({}, bint)
    local borrow = 0
    local wordmaxp1 = BINT_WORDMAX + 1
    for i=1,BINT_SIZE do
      local res = ix[i] + wordmaxp1 - iy[i] - borrow
      z[i] = res & BINT_WORDMAX
      borrow = (res >> BINT_WORDBITS) ~ 1
    end
    return z
  end
  return bint_tonumber(x) - bint_tonumber(y)
end

function bint.__mul(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local z = bint_zero()
    local sizep1 = BINT_SIZE+1
    local s = sizep1
    local e = 0
    for i=1,BINT_SIZE do
      if ix[i] ~= 0 or iy[i] ~= 0 then
        e = math_max(e, i)
        s = math_min(s, i)
      end
    end
    for i=s,e do
      for j=s,math_min(sizep1-i,e) do
        local a = ix[i] * iy[j]
        if a ~= 0 then
          local carry = 0
          for k=i+j-1,BINT_SIZE do
            local tmp = z[k] + (a & BINT_WORDMAX) + carry
            carry = tmp >> BINT_WORDBITS
            z[k] = tmp & BINT_WORDMAX
            a = a >> BINT_WORDBITS
          end
        end
      end
    end
    return z
  end
  return bint_tonumber(x) * bint_tonumber(y)
end

function bint.__eq(x, y)
  for i=1,BINT_SIZE do
    if x[i] ~= y[i] then
      return false
    end
  end
  return true
end

function bint.eq(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    return ix == iy
  end
  return x == y
end
local bint_eq = bint.eq

local function findleftbit(x)
  for i=BINT_SIZE,1,-1 do
    local v = x[i]
    if v ~= 0 then
      local j = 0
      repeat
        v = v >> 1
        j = j + 1
      until v == 0
      return (i-1)*BINT_WORDBITS + j - 1, i
    end
  end
end

local function sudivmod(nume, deno)
  local rema
  local carry = 0
  for i=BINT_SIZE,1,-1 do
    carry = carry | nume[i]
    nume[i] = carry // deno
    rema = carry % deno
    carry = rema << BINT_WORDBITS
  end
  return rema
end

function bint.udivmod(x, y)
  local nume = bint_new(x)
  local deno = bint_assert_convert(y)
  local ishighzero = true
  for i=2,BINT_SIZE do
    if deno[i] ~= 0 then
      ishighzero = false
      break
    end
  end
  if ishighzero then
    local low = deno[1]
    assert(low ~= 0, 'attempt to divide by zero')
    if low == 1 then
      return nume, bint_zero()
    elseif low <= (BINT_WORDMSB - 1) then
      local rema = sudivmod(nume, low)
      return nume, bint_fromuinteger(rema)
    end
  end
  if nume:ult(deno) then
    return bint_zero(), nume
  end
  local denolbit = findleftbit(deno)
  local numelbit, numesize = findleftbit(nume)
  local bit = numelbit - denolbit
  deno = deno << bit
  local wordmaxp1 = BINT_WORDMAX + 1
  local wordbitsm1 = BINT_WORDBITS - 1
  local denosize = numesize
  local quot = bint_zero()
  while bit >= 0 do
    -- compute denominator <= numerator
    local le = true
    local size = math_max(numesize, denosize)
    for i=size,1,-1 do
      local a, b = deno[i], nume[i]
      if a ~= b then
        le = a < b
        break
      end
    end
    -- if the portion of the numerator above the denominator is greater or equal than to the denominator
    if le then
      -- subtract denominator from the portion of the numerator
      local borrow = 0
      for i=1,size do
        local res = nume[i] + wordmaxp1 - deno[i] - borrow
        nume[i] = res & BINT_WORDMAX
        borrow = (res >> BINT_WORDBITS) ~ 1
      end
      -- concatenate 1 to the right bit of the quotient
      local i = (bit // BINT_WORDBITS) + 1
      quot[i] = quot[i] | (1 << (bit % BINT_WORDBITS))
    end
    -- shift right the denominator in one bit
    for i=1,denosize-1 do
      deno[i] = ((deno[i] >> 1) | (deno[i+1] << wordbitsm1)) & BINT_WORDMAX
    end
    local lastdenoword = deno[denosize] >> 1
    deno[denosize] = lastdenoword
    -- recalculate denominator size (optimization)
    if lastdenoword == 0 then
      while deno[denosize] == 0 do
        denosize = denosize - 1
      end
      if denosize == 0 then
        break
      end
    end
    -- decrement current set bit for the quotient
    bit = bit - 1
  end
  -- the remaining numerator is the remainder
  return quot, nume
end
local bint_udivmod = bint.udivmod

--- Perform unsigned division between two integers considering bints.
-- @param x The numerator, must be a bint or a lua integer.
-- @param y The denominator, must be a bint or a lua integer.
-- @return The quotient, a bint.
-- @raise Asserts on attempt to divide by zero
-- or if inputs are not convertible to integers.
function bint.udiv(x, y)
  return (bint_udivmod(x, y))
end

--- Perform unsigned integer modulo operation between two integers considering bints.
-- @param x The numerator, must be a bint or a lua integer.
-- @param y The denominator, must be a bint or a lua integer.
-- @return The remainder, a bint.
-- @raise Asserts on attempt to divide by zero
-- or if the inputs are not convertible to integers.
function bint.umod(x, y)
  local _, rema = bint_udivmod(x, y)
  return rema
end
local bint_umod = bint.umod

--- Perform integer truncate division and modulo operation between two numbers considering bints.
-- This is effectively the same of @{bint.tdiv} and @{bint.tmod}.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient following the remainder, both bint or lua number.
-- @raise Asserts on attempt to divide by zero or on division overflow.
-- @see bint.tdiv
-- @see bint.tmod
function bint.tdivmod(x, y)
  local ax, ay = bint_abs(x), bint_abs(y)
  local ix, iy = tobint(ax), tobint(ay)
  local quot, rema
  if ix and iy then
    assert(not (bint_eq(x, BINT_MININTEGER) and bint_isminusone(y)), 'division overflow')
    quot, rema = bint_udivmod(ix, iy)
  else
    quot, rema = ax // ay, ax % ay
  end
  local isxneg, isyneg = bint_isneg(x), bint_isneg(y)
  if isxneg ~= isyneg then
    quot = -quot
  end
  if isxneg then
    rema = -rema
  end
  return quot, rema
end
local bint_tdivmod = bint.tdivmod

--- Perform truncate division between two numbers considering bints.
-- Truncate division is a division that rounds the quotient towards zero.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient, a bint or lua number.
-- @raise Asserts on attempt to divide by zero or on division overflow.
function bint.tdiv(x, y)
  return (bint_tdivmod(x, y))
end

--- Perform integer truncate modulo operation between two numbers considering bints.
-- The operation is defined as the remainder of the truncate division
-- (division that rounds the quotient towards zero).
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The remainder, a bint or lua number.
-- @raise Asserts on attempt to divide by zero or on division overflow.
function bint.tmod(x, y)
  local _, rema = bint_tdivmod(x, y)
  return rema
end

--- Perform integer floor division and modulo operation between two numbers considering bints.
-- This is effectively the same of @{bint.__idiv} and @{bint.__mod}.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient following the remainder, both bint or lua number.
-- @raise Asserts on attempt to divide by zero.
-- @see bint.__idiv
-- @see bint.__mod
function bint.idivmod(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local isnumeneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local isdenoneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if isnumeneg then
      ix = -ix
    end
    if isdenoneg then
      iy = -iy
    end
    local quot, rema = bint_udivmod(ix, iy)
    if isnumeneg ~= isdenoneg then
      quot:_unm()
      -- round quotient towards minus infinity
      if not rema:iszero() then
        quot:_dec()
        -- adjust the remainder
        if isnumeneg and not isdenoneg then
          rema:_unm():_add(y)
        elseif isdenoneg and not isnumeneg then
          rema:_add(y)
        end
      end
    elseif isnumeneg then
      -- adjust the remainder
      rema:_unm()
    end
    return quot, rema
  end
  local nx, ny = bint_tonumber(x), bint_tonumber(y)
  return nx // ny, nx % ny
end
local bint_idivmod = bint.idivmod

--- Perform floor division between two numbers considering bints.
-- Floor division is a division that rounds the quotient towards minus infinity,
-- resulting in the floor of the division of its operands.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient, a bint or lua number.
-- @raise Asserts on attempt to divide by zero.
function bint.__idiv(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local isnumeneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local isdenoneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if isnumeneg then
      ix = -ix
    end
    if isdenoneg then
      iy = -iy
    end
    local quot, rema = bint_udivmod(ix, iy)
    if isnumeneg ~= isdenoneg then
      quot:_unm()
      -- round quotient towards minus infinity
      if not rema:iszero() then
        quot:_dec()
      end
    end
    return quot, rema
  end
  return bint_tonumber(x) // bint_tonumber(y)
end

--- Perform division between two numbers considering bints.
-- This always casts inputs to floats, for integer division only use @{bint.__idiv}.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient, a lua number.
function bint.__div(x, y)
  return bint_tonumber(x) / bint_tonumber(y)
end

--- Perform integer floor modulo operation between two numbers considering bints.
-- The operation is defined as the remainder of the floor division
-- (division that rounds the quotient towards minus infinity).
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The remainder, a bint or lua number.
-- @raise Asserts on attempt to divide by zero.
function bint.__mod(x, y)
  local _, rema = bint_idivmod(x, y)
  return rema
end

--- Perform integer power between two integers considering bints.
-- If y is negative then pow is performed as an unsigned integer.
-- @param x The base, an integer.
-- @param y The exponent, an integer.
-- @return The result of the pow operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__pow
-- @see bint.upowmod
function bint.ipow(x, y)
  y = bint_assert_convert(y)
  if y:iszero() then
    return bint_one()
  elseif y:isone() then
    return bint_new(x)
  end
  -- compute exponentiation by squaring
  x, y = bint_new(x),  bint_new(y)
  local z = bint_one()
  repeat
    if y:iseven() then
      x = x * x
      y:_shrone()
    else
      z = x * z
      x = x * x
      y:_dec():_shrone()
    end
  until y:isone()
  return x * z
end

--- Perform integer power between two unsigned integers over a modulus considering bints.
-- @param x The base, an integer.
-- @param y The exponent, an integer.
-- @param m The modulus, an integer.
-- @return The result of the pow operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__pow
-- @see bint.ipow
function bint.upowmod(x, y, m)
  m = bint_assert_convert(m)
  if m:isone() then
    return bint_zero()
  end
  x, y = bint_new(x),  bint_new(y)
  local z = bint_one()
  x = bint_umod(x, m)
  while not y:iszero() do
    if y:isodd() then
      z = bint_umod(z*x, m)
    end
    y:_shrone()
    x = bint_umod(x*x, m)
  end
  return z
end

--- Perform numeric power between two numbers considering bints.
-- This always casts inputs to floats, for integer power only use @{bint.ipow}.
-- @param x The base, a bint or lua number.
-- @param y The exponent, a bint or lua number.
-- @return The result of the pow operation, a lua number.
-- @see bint.ipow
function bint.__pow(x, y)
  return bint_tonumber(x) ^ bint_tonumber(y)
end

--- Bitwise left shift integers considering bints.
-- @param x An integer to perform the bitwise shift.
-- @param y An integer with the number of bits to shift.
-- @return The result of shift operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__shl(x, y)
  x, y = bint_new(x), bint_assert_tointeger(y)
  if y == math_mininteger or math_abs(y) >= BINT_BITS then
    return bint_zero()
  end
  if y < 0 then
    return x >> -y
  end
  local nvals = y // BINT_WORDBITS
  if nvals ~= 0 then
    x:_shlwords(nvals)
    y = y - nvals * BINT_WORDBITS
  end
  if y ~= 0 then
    local wordbitsmy = BINT_WORDBITS - y
    for i=BINT_SIZE,2,-1 do
      x[i] = ((x[i] << y) | (x[i-1] >> wordbitsmy)) & BINT_WORDMAX
    end
    x[1] = (x[1] << y) & BINT_WORDMAX
  end
  return x
end

function bint.__shr(x, y)
  x, y = bint_new(x), bint_assert_tointeger(y)
  if y == math_mininteger or math_abs(y) >= BINT_BITS then
    return bint_zero()
  end
  if y < 0 then
    return x << -y
  end
  local nvals = y // BINT_WORDBITS
  if nvals ~= 0 then
    x:_shrwords(nvals)
    y = y - nvals * BINT_WORDBITS
  end
  if y ~= 0 then
    local wordbitsmy = BINT_WORDBITS - y
    for i=1,BINT_SIZE-1 do
      x[i] = ((x[i] >> y) | (x[i+1] << wordbitsmy)) & BINT_WORDMAX
    end
    x[BINT_SIZE] = x[BINT_SIZE] >> y
  end
  return x
end

--- Bitwise AND bints (in-place).
-- @param y An integer to perform bitwise AND.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_band(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = self[i] & y[i]
  end
  return self
end

--- Bitwise AND two integers considering bints.
-- @param x An integer to perform bitwise AND.
-- @param y An integer to perform bitwise AND.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__band(x, y)
  return bint_new(x):_band(y)
end

--- Bitwise OR bints (in-place).
-- @param y An integer to perform bitwise OR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_bor(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = self[i] | y[i]
  end
  return self
end

--- Bitwise OR two integers considering bints.
-- @param x An integer to perform bitwise OR.
-- @param y An integer to perform bitwise OR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__bor(x, y)
  return bint_new(x):_bor(y)
end

--- Bitwise XOR bints (in-place).
-- @param y An integer to perform bitwise XOR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_bxor(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = self[i] ~ y[i]
  end
  return self
end

function bint.__bxor(x, y)
  return bint_new(x):_bxor(y)
end

--- Bitwise NOT a bint (in-place).
function bint:_bnot()
  for i=1,BINT_SIZE do
    self[i] = (~self[i]) & BINT_WORDMAX
  end
  return self
end

function bint.__bnot(x)
  local y = setmetatable({}, bint)
  for i=1,BINT_SIZE do
    y[i] = (~x[i]) & BINT_WORDMAX
  end
  return y
end

function bint:_unm()
  return self:_bnot():_inc()
end

function bint.__unm(x)
  return (~x):_inc()
end

function bint.ult(x, y)
  x, y = bint_assert_convert(x), bint_assert_convert(y)
  for i=BINT_SIZE,1,-1 do
    local a, b = x[i], y[i]
    if a ~= b then
      return a < b
    end
  end
  return false
end

function bint.ule(x, y)
  x, y = bint_assert_convert(x), bint_assert_convert(y)
  for i=BINT_SIZE,1,-1 do
    local a, b = x[i], y[i]
    if a ~= b then
      return a < b
    end
  end
  return true
end

function bint.__lt(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local xneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local yneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if xneg == yneg then
      for i=BINT_SIZE,1,-1 do
        local a, b = ix[i], iy[i]
        if a ~= b then
          return a < b
        end
      end
      return false
    end
    return xneg and not yneg
  end
  return bint_tonumber(x) < bint_tonumber(y)
end

function bint.__le(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local xneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local yneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if xneg == yneg then
      for i=BINT_SIZE,1,-1 do
        local a, b = ix[i], iy[i]
        if a ~= b then
          return a < b
        end
      end
      return true
    end
    return xneg and not yneg
  end
  return bint_tonumber(x) <= bint_tonumber(y)
end

function bint:__tostring()
  return self:tobase(10)
end

setmetatable(bint, {
  __call = function(_, x)
    return bint_new(x)
  end
})

BINT_MATHMININTEGER, BINT_MATHMAXINTEGER = bint_new(math.mininteger), bint_new(math.maxinteger)
BINT_MININTEGER = bint.mininteger()
memo[memoindex] = bint

return bint

end

return newmodule