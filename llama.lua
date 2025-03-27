-- bint
local function luainteger_bitsize()
    local n, i = -1, 0
    repeat
        n, i = n >> 16, i + 16
    until n == 0
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
    assert(2 * wordbits <= intbits, 'word bitsize must be half of the lua integer bitsize')
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
    local BINT_LEPACKFMT = '<' .. ('I' .. (wordbits // 8)):rep(BINT_SIZE)
    local BINT_MATHMININTEGER, BINT_MATHMAXINTEGER
    local BINT_MININTEGER

    function bint.zero()
        local x = setmetatable({}, bint)
        for i = 1, BINT_SIZE do
            x[i] = 0
        end
        return x
    end

    local bint_zero = bint.zero

    function bint.one()
        local x = setmetatable({}, bint)
        x[1] = 1
        for i = 2, BINT_SIZE do
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
            for i = 1, BINT_SIZE do
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
            for i = 1, BINT_SIZE do
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
        return ipow(x * y, x * x, (n - 1) // 2)
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
        for i = 1, #int, step do
            local part = int:sub(i, i + step - 1)
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
            buffer = buffer .. ('\x00'):rep(BINT_BYTES - #buffer)
        end
        return setmetatable({ BINT_LEPACKFMT:unpack(buffer) }, bint)
    end

    function bint.frombe(buffer)
        assert(type(buffer) == 'string', 'buffer is not a string')
        if #buffer > BINT_BYTES then
            buffer = buffer:sub(-BINT_BYTES, #buffer)
        elseif #buffer < BINT_BYTES then
            buffer = ('\x00'):rep(BINT_BYTES - #buffer) .. buffer
        end
        return setmetatable({ BINT_LEPACKFMT:unpack(buffer:reverse()) }, bint)
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
        for i = 1, BINT_SIZE do
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
            for i = 1, BINT_SIZE do
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
            for i = 1, BINT_SIZE do
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
            for i = 1, BINT_SIZE do
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
        for i = 1, 36 do
            BASE_LETTERS[i - 1] = ('0123456789abcdefghijklmnopqrstuvwxyz'):sub(i, i)
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
            for i = size, 1, -1 do
                carry = carry | x[i]
                d, xd = carry // basepow, carry % basepow
                if xiszero and d ~= 0 then
                    size = i
                    xiszero = false
                end
                x[i] = d
                carry = xd << BINT_WORDBITS
            end
            for _ = 1, step do
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
            for i = 1, BINT_SIZE do
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
            for i = 2, BINT_SIZE do
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
            for i = 1, BINT_SIZE do
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
        for i = 1, BINT_SIZE - 1 do
            x[i] = BINT_WORDMAX
        end
        x[BINT_SIZE] = BINT_WORDMAX ~ BINT_WORDMSB
        return x
    end

    function bint.mininteger()
        local x = setmetatable({}, bint)
        for i = 1, BINT_SIZE - 1 do
            x[i] = 0
        end
        x[BINT_SIZE] = BINT_WORDMSB
        return x
    end

    function bint:_shlone()
        local wordbitsm1 = BINT_WORDBITS - 1
        for i = BINT_SIZE, 2, -1 do
            self[i] = ((self[i] << 1) | (self[i - 1] >> wordbitsm1)) & BINT_WORDMAX
        end
        self[1] = (self[1] << 1) & BINT_WORDMAX
        return self
    end

    function bint:_shrone()
        local wordbitsm1 = BINT_WORDBITS - 1
        for i = 1, BINT_SIZE - 1 do
            self[i] = ((self[i] >> 1) | (self[i + 1] << wordbitsm1)) & BINT_WORDMAX
        end
        self[BINT_SIZE] = self[BINT_SIZE] >> 1
        return self
    end

    function bint:_shlwords(n)
        for i = BINT_SIZE, n + 1, -1 do
            self[i] = self[i - n]
        end
        for i = 1, n do
            self[i] = 0
        end
        return self
    end

    function bint:_shrwords(n)
        if n < BINT_SIZE then
            for i = 1, BINT_SIZE - n do
                self[i] = self[i + n]
            end
            for i = BINT_SIZE - n + 1, BINT_SIZE do
                self[i] = 0
            end
        else
            for i = 1, BINT_SIZE do
                self[i] = 0
            end
        end
        return self
    end

    function bint:_inc()
        for i = 1, BINT_SIZE do
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
        for i = 1, BINT_SIZE do
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
        for i = 1, BINT_SIZE do
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
                x:bror(-(y + 1))
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
                x:brol(-(y + 1))
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
        for i = 1, BINT_SIZE do
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
            for i = 1, BINT_SIZE do
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
        for i = 1, BINT_SIZE do
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
            for i = 1, BINT_SIZE do
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
            local sizep1 = BINT_SIZE + 1
            local s = sizep1
            local e = 0
            for i = 1, BINT_SIZE do
                if ix[i] ~= 0 or iy[i] ~= 0 then
                    e = math_max(e, i)
                    s = math_min(s, i)
                end
            end
            for i = s, e do
                for j = s, math_min(sizep1 - i, e) do
                    local a = ix[i] * iy[j]
                    if a ~= 0 then
                        local carry = 0
                        for k = i + j - 1, BINT_SIZE do
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
        for i = 1, BINT_SIZE do
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
        for i = BINT_SIZE, 1, -1 do
            local v = x[i]
            if v ~= 0 then
                local j = 0
                repeat
                    v = v >> 1
                    j = j + 1
                until v == 0
                return (i - 1) * BINT_WORDBITS + j - 1, i
            end
        end
    end

    local function sudivmod(nume, deno)
        local rema
        local carry = 0
        for i = BINT_SIZE, 1, -1 do
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
        for i = 2, BINT_SIZE do
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
            local le = true
            local size = math_max(numesize, denosize)
            for i = size, 1, -1 do
                local a, b = deno[i], nume[i]
                if a ~= b then
                    le = a < b
                    break
                end
            end
            if le then
                local borrow = 0
                for i = 1, size do
                    local res = nume[i] + wordmaxp1 - deno[i] - borrow
                    nume[i] = res & BINT_WORDMAX
                    borrow = (res >> BINT_WORDBITS) ~ 1
                end
                local i = (bit // BINT_WORDBITS) + 1
                quot[i] = quot[i]| (1 << (bit % BINT_WORDBITS))
            end
            for i = 1, denosize - 1 do
                deno[i] = ((deno[i] >> 1) | (deno[i + 1] << wordbitsm1)) & BINT_WORDMAX
            end
            local lastdenoword = deno[denosize] >> 1
            deno[denosize] = lastdenoword
            if lastdenoword == 0 then
                while deno[denosize] == 0 do
                    denosize = denosize - 1
                end
                if denosize == 0 then
                    break
                end
            end
            bit = bit - 1
        end
        return quot, nume
    end

    local bint_udivmod = bint.udivmod

    function bint.udiv(x, y)
        return (bint_udivmod(x, y))
    end

    function bint.umod(x, y)
        local _, rema = bint_udivmod(x, y)
        return rema
    end

    local bint_umod = bint.umod

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

    function bint.tdiv(x, y)
        return (bint_tdivmod(x, y))
    end

    function bint.tmod(x, y)
        local _, rema = bint_tdivmod(x, y)
        return rema
    end

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

    function bint.__div(x, y)
        return bint_tonumber(x) / bint_tonumber(y)
    end

    function bint.__mod(x, y)
        local _, rema = bint_idivmod(x, y)
        return rema
    end

    function bint.ipow(x, y)
        y = bint_assert_convert(y)
        if y:iszero() then
            return bint_one()
        elseif y:isone() then
            return bint_new(x)
        end
        -- compute exponentiation by squaring
        x, y = bint_new(x), bint_new(y)
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

    function bint.upowmod(x, y, m)
        m = bint_assert_convert(m)
        if m:isone() then
            return bint_zero()
        end
        x, y = bint_new(x), bint_new(y)
        local z = bint_one()
        x = bint_umod(x, m)
        while not y:iszero() do
            if y:isodd() then
                z = bint_umod(z * x, m)
            end
            y:_shrone()
            x = bint_umod(x * x, m)
        end
        return z
    end

    function bint.__pow(x, y)
        return bint_tonumber(x) ^ bint_tonumber(y)
    end

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
            for i = BINT_SIZE, 2, -1 do
                x[i] = ((x[i] << y) | (x[i - 1] >> wordbitsmy)) & BINT_WORDMAX
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
            for i = 1, BINT_SIZE - 1 do
                x[i] = ((x[i] >> y) | (x[i + 1] << wordbitsmy)) & BINT_WORDMAX
            end
            x[BINT_SIZE] = x[BINT_SIZE] >> y
        end
        return x
    end

    function bint:_band(y)
        y = bint_assert_convert(y)
        for i = 1, BINT_SIZE do
            self[i] = self[i] & y[i]
        end
        return self
    end

    function bint.__band(x, y)
        return bint_new(x):_band(y)
    end

    function bint:_bor(y)
        y = bint_assert_convert(y)
        for i = 1, BINT_SIZE do
            self[i] = self[i]| y[i]
        end
        return self
    end

    function bint.__bor(x, y)
        return bint_new(x):_bor(y)
    end

    function bint:_bxor(y)
        y = bint_assert_convert(y)
        for i = 1, BINT_SIZE do
            self[i] = self[i] ~ y[i]
        end
        return self
    end

    function bint.__bxor(x, y)
        return bint_new(x):_bxor(y)
    end

    --- Bitwise NOT a bint (in-place).
    function bint:_bnot()
        for i = 1, BINT_SIZE do
            self[i] = (~self[i]) & BINT_WORDMAX
        end
        return self
    end

    function bint.__bnot(x)
        local y = setmetatable({}, bint)
        for i = 1, BINT_SIZE do
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
        for i = BINT_SIZE, 1, -1 do
            local a, b = x[i], y[i]
            if a ~= b then
                return a < b
            end
        end
        return false
    end

    function bint.ule(x, y)
        x, y = bint_assert_convert(x), bint_assert_convert(y)
        for i = BINT_SIZE, 1, -1 do
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
                for i = BINT_SIZE, 1, -1 do
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
                for i = BINT_SIZE, 1, -1 do
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

-- CRYPTO
local bint = newmodule(4096) -- for secp256k1

-- doing what we can for public safety in Lua
function AssertWord(word)
    assert(#word == 64 and word:match("^[01]*$"), "word must be a binary string of length == 64")
end

function ByteToBits(byte)
    local bits = ""
    for i = 0, 7 do
        local bit = math.floor(byte / (2^i)) % 2
        bits = bits .. bit
    end
    return bits
end

function StringToBitstring(s)
    local bitstring = ""
    for i = 1, #s do
        local byte = s:byte(i)
        bitstring = bitstring .. ByteToBits(byte)
    end
    return bitstring
end

function CreateLanes(bitString)
    local block = {}
    for i = 1, #bitString, 64 do
        table.insert(block, string.sub(bitString, i, i+63))
    end

    return block
end

-- string helpers
function MakeBlocks(stringInput)
    local bitString = StringToBitstring(stringInput)
    local blocks = {}
    -- take out full chunks while we can
    while #bitString >= 1088 do
        local block = CreateLanes(string.sub(bitString, 1, 1088)) -- block is a table of length 17 full of words of length 64
        table.insert(blocks, block) -- blocks is a table 
        bitString = string.sub(bitString, 1089)
    end
    if #bitString <= 1086 then
        bitString = bitString .. "1" .. string.rep("0", 1086 - #bitString) .. "1"
        local block = CreateLanes(bitString) -- perfect fit
        table.insert(blocks, block)
        bitString = ""
    else -- on the off chance that we end up with 1087 or 1088 left over, we have to make another block for the padding
        bitString = bitString .. "1" .. string.rep("0", 2174 - #bitString) .. "1"
        local block = CreateLanes(string.sub(bitString, 1, 1088))
        table.insert(blocks, block)
        bitString = string.sub(bitString, 1089)
        block = CreateLanes(bitString)
        table.insert(blocks, block)
    end

    return blocks
end

function ReverseBitsInByte(byteStr)
    assert(#byteStr == 8)
    return string.reverse(byteStr)
end

function NormalizeBinaryEndian(bitString)
    local fixed = {}
    for i = 1, #bitString, 8 do
        table.insert(fixed, ReverseBitsInByte(bitString:sub(i, i+7)))
    end
    return table.concat(fixed)
end

function MakeBinaryBlocks(bitString)
    local bitString = bitString
    bitString = NormalizeBinaryEndian(bitString) 
    local blocks = {}
    -- take out full chunks while we can
    while #bitString >= 1088 do
        local block = CreateLanes(string.sub(bitString, 1, 1088)) -- block is a table of length 17 full of words of length 64
        table.insert(blocks, block) -- blocks is a table 
        bitString = string.sub(bitString, 1089)
    end
    if #bitString <= 1086 then
        bitString = bitString .. "1" .. string.rep("0", 1086 - #bitString) .. "1"
        local block = CreateLanes(bitString) -- perfect fit
        table.insert(blocks, block)
        bitString = ""
    else -- on the off chance that we end up with 1087 or 1088 left over, we have to make another block for the padding
        bitString = bitString .. "1" .. string.rep("0", 2174 - #bitString) .. "1"
        local block = CreateLanes(string.sub(bitString, 1, 1088))
        table.insert(blocks, block)
        bitString = string.sub(bitString, 1089)
        block = CreateLanes(bitString)
        table.insert(blocks, block)
    end

    return blocks
end

function DecimalToBinary(decimalStr)
    if decimalStr == "0" then
        return "0"
    end

    local binary = ""
    local remainder = 0

    while decimalStr ~= "0" do
        local newDecimal = ""
        remainder = 0

        for i = 1, #decimalStr do
            local digit = tonumber(decimalStr:sub(i, i))
            local currentValue = remainder * 10 + digit
            newDecimal = newDecimal .. math.floor(currentValue / 2)
            remainder = currentValue % 2
        end

        newDecimal = newDecimal:gsub("^0+", "")
        if newDecimal == "" then newDecimal = "0" end

        binary = tostring(remainder) .. binary

        decimalStr = newDecimal
    end

    return binary
end

function BinaryWordToHex(word)
    local result = ""
    for i = 1, 64, 4 do
        local nibble = word:sub(i, i+3)
        local dec = tonumber(nibble, 2)
        result = result .. string.format("%x", dec)
    end

    return result
end

function DecimalToHex(decimalStr)
    if decimalStr == "0" then
        return "0"
    end

    local binary = DecimalToBinary(decimalStr)

    local padding = (4 - (#binary % 4)) % 4
    binary = string.rep("0", padding) .. binary

    local result = ""
    for i = 1, #binary, 4 do
        local nibble = binary:sub(i, i+3)
        local dec = tonumber(nibble, 2)
        result = result .. string.format("%x", dec)
    end

    return result
end

local function BoolToString(boolIn)
    assert(type(boolIn) == 'boolean', 'expected type `bool` but got ' .. type(boolIn))
    if boolIn then
        return "1"
    end
    return "0"
end

function HexToBinary(hexString)
    local result = ""
    for i = 1, #hexString do
        local digit = hexString:sub(i,i)
        local decimal = tonumber(digit, 16)
        if not decimal then
            error("invalid hex digit: " .. digit)
        end
        
        local binary = ""
        for j = 1, 4 do
            binary = (decimal % 2) .. binary
            decimal = math.floor(decimal / 2)
        end
        result = result .. binary
    end

    return result
end

local function LeftPad(inputString, targetLength)
    return string.rep("0", targetLength - #inputString) .. inputString
end

-- bitwise op utils
function BitwiseXor(word1, word2)
    AssertWord(word1)
    AssertWord(word2)

    local result = ""
    for i = 1, 64 do
        local bit1 = word1:sub(i,i)
        local bit2 = word2:sub(i,i)
        if bit1 ~= bit2 then
            result = result .. "1"
        else
            result = result .. "0"
        end
    end

    return result
end

function BitwiseAnd(word1, word2)
    AssertWord(word1)
    AssertWord(word2)

    local result = ""
    for i = 1, 64 do
        local bit1 = word1:sub(i,i)
        local bit2 = word2:sub(i,i)
        if bit1 == "1" and bit2 == "1" then
            result = result .. "1"
        else
            result = result .. "0"
        end
    end

    return result
end

function BitwiseOr(word1, word2)
    AssertWord(word1)
    AssertWord(word2)

    local result = ""
    for i = 1, 64 do
        local bit1 = word1:sub(i,i)
        local bit2 = word2:sub(i,i)
        if bit1 == "1" or bit2 == "1" then
            result = result .. "1"
        else
            result = result .. "0"
        end
    end

    return result
end

function BitwiseNot(word)
    AssertWord(word)

    local result = ""
    for i = 1, 64 do
        local bit = word:sub(i,i)
        if bit == "1" then
            result = result .. "0"
        else
            result = result .. "1"
        end
    end
    return result
end

function RotateLeft(word, offset)
    offset = offset % #word  -- Handle rotations larger than string length
    if offset == 0 then
        return word
    end
    return string.sub(word, #word - offset + 1) .. string.sub(word, 1, #word - offset)
end

RoundConstants = {
    '1000000000000000000000000000000000000000000000000000000000000000',
    '0100000100000001000000000000000000000000000000000000000000000000',
    '0101000100000001000000000000000000000000000000000000000000000001',
    '0000000000000001000000000000000100000000000000000000000000000001',
    '1101000100000001000000000000000000000000000000000000000000000000',
    '1000000000000000000000000000000100000000000000000000000000000000',
    '1000000100000001000000000000000100000000000000000000000000000001',
    '1001000000000001000000000000000000000000000000000000000000000001',
    '0101000100000000000000000000000000000000000000000000000000000000',
    '0001000100000000000000000000000000000000000000000000000000000000',
    '1001000000000001000000000000000100000000000000000000000000000000',
    '0101000000000000000000000000000100000000000000000000000000000000',
    '1101000100000001000000000000000100000000000000000000000000000000',
    '1101000100000000000000000000000000000000000000000000000000000001',
    '1001000100000001000000000000000000000000000000000000000000000001',
    '1100000000000001000000000000000000000000000000000000000000000001',
    '0100000000000001000000000000000000000000000000000000000000000001',
    '0000000100000000000000000000000000000000000000000000000000000001',
    '0101000000000001000000000000000000000000000000000000000000000000',
    '0101000000000000000000000000000100000000000000000000000000000001',
    '1000000100000001000000000000000100000000000000000000000000000001',
    '0000000100000001000000000000000000000000000000000000000000000001',
    '1000000000000000000000000000000100000000000000000000000000000000',
    '0001000000000001000000000000000100000000000000000000000000000001'
}

local Keccak = {}
-- this may be better without classes, which are not natural in Lua anyway, so I may refactor
Keccak.__index = Keccak

function Keccak.new()
    local self = setmetatable({}, Keccak)

    local repeatedZero = string.rep("0", 64)

    self.state = {}

    for i = 1, 5 do
        self.state[i] = {}
        for j = 1, 5 do
            self.state[i][j] = repeatedZero
        end
    end

    return self
end

-- genuine Keccak functions
function Keccak:absorb(blocks)
    assert(#blocks == 17, "to absorb we must provide 1088 bits, matching the rate.")
    for _, word in ipairs(blocks) do
        AssertWord(word)
    end

    for i = 1, 17 do
        local x = ((i-1) % 5) + 1
        local y = ((i-1) // 5) + 1
        self.state[x][y] = BitwiseXor(self.state[x][y], blocks[i])
    end
end

function Keccak:squeeze()
    local binaryString = self.state[1][1] .. self.state[2][1] .. self.state[3][1] .. self.state[4][1]

    -- undoes endian error somewhere in the guts...no idea where...will revisit
    local reversedByteTable = {} 
    for i = 1, #binaryString, 8 do
        local reversedByte = string.reverse(string.sub(binaryString, i, i+7))
        table.insert(reversedByteTable, reversedByte)
    end

    return table.concat(reversedByteTable)
end

function Keccak:theta()
    local C = {}
    local D = {}

    for x = 1, 5 do
        C[x] = self.state[x][1]
        for y = 2, 5 do
            C[x] = BitwiseXor(C[x], self.state[x][y])
        end
    end

    for x = 1, 5 do
        local x_minus_1 = ((x - 2) % 5) + 1
        local x_plus_1  = (x % 5) + 1
        D[x] = BitwiseXor(C[x_minus_1], RotateLeft(C[x_plus_1], 1))
    end

    for x = 1, 5 do
        for y = 1, 5 do
            self.state[x][y] = BitwiseXor(self.state[x][y], D[x])
        end
    end
end

function Keccak:rho()
    local rhoOffsets = {
        { 0, 36,  3, 41, 18},
        { 1, 44, 10, 45,  2},
        {62,  6, 43, 15, 61},
        {28, 55, 25, 21, 56},
        {27, 20, 39,  8, 14}
    }

    for x = 1, 5 do
        for y = 1, 5 do
            self.state[x][y] = RotateLeft(self.state[x][y], rhoOffsets[x][y])
        end
    end
end

function Keccak:pi()
    local B = {}
    for i = 1, 5 do
      B[i] = {}
    end
  
    for x = 1, 5 do
      for y = 1, 5 do
        local X = x - 1  -- 0-indexed x
        local Y = y - 1  -- 0-indexed y
  
        local newX = Y
        local newY = (2 * X + 3 * Y) % 5
  
        B[newX + 1][newY + 1] = self.state[x][y]
      end
    end
  
    for x = 1, 5 do
        for y = 1, 5 do
            self.state[x][y] = B[x][y]
        end
    end
end

function Keccak:chi()
    local newState = {}
    for x = 1, 5 do
      newState[x] = {}
    end

    for y = 1, 5 do
      for x = 1, 5 do
        local current = self.state[x][y]
        local next1   = self.state[(x % 5) + 1][y]
        local next2   = self.state[(x + 1) % 5 + 1][y]

        local not_next1 = BitwiseNot(next1)
        local and_part  = BitwiseAnd(not_next1, next2)
 
        newState[x][y] = BitwiseXor(current, and_part)
      end
    end

    for x = 1, 5 do
        for y = 1, 5 do
            self.state[x][y] = newState[x][y]
        end
    end

    return newState
end

function Keccak:iota(round)
    self.state[1][1] = BitwiseXor(self.state[1][1], RoundConstants[round])
end

function Keccak:f_perm()
    for i = 1, 24 do
        self:theta()
        self:rho()
        self:pi()
        self:chi()
        self:iota(i)
    end
end

function Hash(stringInput)
    assert(type(stringInput) == "string", "conventional support only provided for hashes of strings")
    local k = Keccak.new()

    local blocks = MakeBlocks(stringInput)
    for _, block in ipairs(blocks) do
        k:absorb(block)
        k:f_perm()
    end
    local result = k:squeeze()

    result = BinaryWordToHex(result:sub(1, 64)) .. BinaryWordToHex(result:sub(65, 128)) .. BinaryWordToHex(result:sub(129, 192)) .. BinaryWordToHex(result:sub(193, 256)) -- hex string of length 64

    return result
end

function HashBinary(stringInput)
    local k = Keccak.new()

    local blocks = MakeBinaryBlocks(stringInput)
    for _, block in ipairs(blocks) do
        k:absorb(block)
        k:f_perm()
    end
    local result = k:squeeze()

    result = BinaryWordToHex(result:sub(1, 64)) .. BinaryWordToHex(result:sub(65, 128)) .. BinaryWordToHex(result:sub(129, 192)) .. BinaryWordToHex(result:sub(193, 256)) -- hex string of length 64

    return result
end

function GeneratePrivateKey()
    -- strong intuition that I would get smoked on StackOverflow for this
    -- but we don't have crypto libraries, nor io, and aos's random function doesn't seem to work, so this is what we got
    math.randomseed(math.floor(os.time() * 1000 + os.clock() * 1000))

    local privateKeyTable = {}
    for _ = 1, 32 do
        local randHex = math.random(0, 15)
        table.insert(privateKeyTable, string.format("%x", randHex))
    end

    local result = table.concat(privateKeyTable)

    result = Hash(result)

    return result
end

-- secp256k1 implementation nearly completely from https://github.com/andreacorbellini/ecc, which and whose accompanying blog series are A+

local EllipticCurveAlgorithm = {}
EllipticCurveAlgorithm.__index = EllipticCurveAlgorithm

function EllipticCurveAlgorithm.new(name, p, a, b, g_x, g_y, n, h)
    local self = setmetatable({}, EllipticCurveAlgorithm)
    self.name = name
    self.p = p
    self.a = a
    self.b = b
    self.g_x = g_x
    self.g_y = g_y
    self.n = n
    self.h = h

    return self
end

function EllipticCurveAlgorithm:isPointOnCurve(point)
    if point == nil then -- nil for the point at infinity; this is guaranteed to lead to some bug if I don't find a clearer way of doing it soon
        return true
    end
    
    local x, y = table.unpack(point)

    local val = (((y * y) - (x * x * x) - (self.a * x) - self.b) % self.p) -- just the curve equation
    return val:eq(0)
end

function CurveNegate(algorithm, point)
    assert(algorithm:isPointOnCurve(point), 'input point not on curve.')

    if point == nil then
        return nil
    end

    local x, y = table.unpack(point)
    local newY = -y % algorithm.p
    local newPoint = {x, newY}

    assert(algorithm:isPointOnCurve(newPoint), 'error in calculation of negation of point.')

    return newPoint
end

local function InverseMod(k, p)
    if k == 0 then
        error('division by zero')
    end

    if k < 0 then
        return p - InverseMod(-k, p)
    end

    local s, old_s = 0, 1
    local t, old_t = 1, 0
    local r, old_r = p, k

    while not r:eq(0) do
        local quotient = old_r // r
        old_r, r = r, old_r - quotient * r
        old_s, s = s, old_s - quotient * s
        old_t, t = t, old_t - quotient * t
    end

    local gcd, x, y = old_r, old_s, old_t
    assert(gcd:eq(1))
    assert(((k * x) % p):eq(1))

    return (x % p)
end

local function CurveAdd(algorithm, point1, point2)
    -- note that we pass in the algorithm because the curve coefficients are necessary for this operation
    -- it would be easier to hard-code this for now, but the dead parameter will make it easier to support other curves down the line
    assert(algorithm:isPointOnCurve(point1))
    assert(algorithm:isPointOnCurve(point2))

    if point1 == nil then
        return point2
    end
    if point2 == nil then
        return point1
    end

    local x1, y1 = table.unpack(point1)
    local x2, y2 = table.unpack(point2)

    if x1:eq(x2) and not y1:eq(y2) then
        return nil
    end

    local m
    if x1:eq(x2) then
        m = (3 * x1 * x1 + algorithm.a) * InverseMod(2 * y1, algorithm.p)
    else
        m = (y1 - y2) * InverseMod(x1 - x2, algorithm.p)
    end

    local x3 = m * m - x1 - x2
    local y3 = y1 + m * (x3 - x1)
    local result = {x3 % algorithm.p, -y3 % algorithm.p}
    assert(algorithm:isPointOnCurve(result))

    return result
end

local function ScalarCurveMultiply(algorithm, k, point)
    assert(algorithm:isPointOnCurve(point))

    if (k % algorithm.n):eq(0) or point == nil then
        return nil
    end

    if k < 0 then
        return ScalarCurveMultiply(-k, CurveNegate(point))
    end

    local result = nil
    local addend = point

    -- double and add
    k = tostring(k)
    k = DecimalToBinary(k)
    while k ~= "0" and k ~= "" do
        if k:sub(-1) == "1" then
            result = CurveAdd(algorithm, result, addend)
        end

        addend = CurveAdd(algorithm, addend, addend)

        k = k:sub(1, -2)
    end

    assert(algorithm:isPointOnCurve(result))

    return result
end

local function Secp256k1()
    -- makes it easier to get the curve we care about here
    local algorithm = EllipticCurveAlgorithm.new("secp256k1",
        bint.fromstring("0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f"), -- p (field order)
        bint.fromstring("0x0"), -- a
        bint.fromstring("0x7"), -- b
        bint.fromstring("0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"), -- g_x
        bint.fromstring("0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"), -- g_y
        bint.fromstring("0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141"), -- n
        bint.fromstring("0x1") -- h
    )

    return algorithm
end

local function MakeCredentials() -- all in one for an ethereum wallet
    ::continue::
    local algorithm = Secp256k1()
    local privateKey = GeneratePrivateKey()
    local privateKeyNumber = bint.fromstring('0x' .. privateKey)
    local publicCurvePoint = ScalarCurveMultiply(algorithm, privateKeyNumber, {algorithm.g_x, algorithm.g_y})
    if publicCurvePoint == nil then -- for the linter; this is impossible; to be re-implemented
        goto continue
    end
    local x, y = table.unpack(publicCurvePoint)
    publicCurvePoint = {x % algorithm.p, y % algorithm.p}
    local publicKey = LeftPad(DecimalToHex(tostring(publicCurvePoint[1])), 64).. LeftPad(DecimalToHex(tostring(publicCurvePoint[2])), 64)
    local publicKeyBinary = HexToBinary(publicKey)
    local address = string.sub(HashBinary(publicKeyBinary), -40)

    return privateKey, publicKey, address
end

local function SignMessage(privateKey, hash)
    local algorithm = Secp256k1()
    local z = bint.fromstring('0x' .. hash)

    local r = bint.new(0)
    local s = bint.new(0)

    local yParity = ""
    while r:eq(0) or s:eq(0) do
        ::continue::
        local k = bint.fromstring('0x' .. Hash(hash))
        k = k % algorithm.n -- 2^256 \mod algorithm.n \neq 0, so this is not a buttoned-up way to do it; to be reimplemented

        local curvePoint = ScalarCurveMultiply(algorithm, k, {algorithm.g_x, algorithm.g_y})
        if curvePoint == nil then
            goto continue
        end
        local x, y = table.unpack(curvePoint)

        r = x % algorithm.n
        s = ((z + r * privateKey) * InverseMod(k, algorithm.n)) % algorithm.n
        yParity = y % 2
    end

    if s > algorithm.n / 2 then
        s = algorithm.n - s
        yParity = 1 - yParity
    end

    return r, s, tostring(yParity)
end

local function CalculateContribution(hexString)
    if hexString == "" or hexString:gsub("0", "") == "" then
        return "80"
    end
    local hexString = string.rep('0', math.ceil(#hexString / 2) * 2 - #hexString) .. hexString
    if tonumber(hexString, 16) <= 127 then
        return hexString
    else
        local numBytes = math.floor(#hexString / 2)
        return DecimalToHex(tostring(128 + numBytes)) .. hexString
    end
end

local function HexStringByteLength(hexString)
    return math.floor(math.ceil(#hexString / 2))
end

local function GenerateRlpListPrefix(rlpEncodedTransaction)
    local transactionLengthNumber = HexStringByteLength(rlpEncodedTransaction)
    local transactionLengthHexString = DecimalToHex(tostring(transactionLengthNumber))
    local listPrefix = ""
    if transactionLengthNumber >= 55 then
        local transactionLengthLengthNumber = HexStringByteLength(transactionLengthHexString)
        listPrefix = DecimalToHex(tostring(247 + transactionLengthLengthNumber))
        local lengthPrefix = LeftPad(transactionLengthHexString, 2)
        listPrefix = listPrefix .. lengthPrefix
    else
        listPrefix = DecimalToHex(tostring(192 + transactionLengthNumber))
    end

    return listPrefix
end

local function RlpEncodeRawTransaction(nonce, destination, amount)
    -- several values are hard-coded for the purposes of this demo
    local chainIdContribution = "01"
    local nonceContribution = CalculateContribution(nonce)
    local maxPriorityFeePerGasContribution = "8501DCD65000"
    local maxFeePerGasContribution = "8502CB417800"
    local gasLimitContribution = "825208"
    local destinationContribution = "94" .. destination
    local amountContribution = CalculateContribution(amount)
    local dataContribution = "80"
    local accessListContribution = "c0"

    local outString = chainIdContribution .. nonceContribution .. maxPriorityFeePerGasContribution .. maxFeePerGasContribution .. gasLimitContribution .. destinationContribution .. amountContribution .. dataContribution .. accessListContribution

    outString = '02' .. GenerateRlpListPrefix(outString) .. outString

    return outString
end

function SendEthereum(privateKey, nonce, destination)
    local amount = 'DA475ABF0000'
    local privateKeybint = bint.fromstring('0x' .. privateKey)

    local encodedTransaction = RlpEncodeRawTransaction(nonce, destination, amount):lower()
    local binaryEncodedTransaction = HexToBinary(encodedTransaction)
    local digest = HashBinary(binaryEncodedTransaction)

    local r, s, yParity = SignMessage(privateKeybint, digest)
    local r = LeftPad(DecimalToHex(tostring(r)), 64) -- we no longer need them as bints; `local` needed because we are changing type from `table`
    local s = LeftPad(DecimalToHex(tostring(s)), 64)

    local strippedTransaction = string.sub(encodedTransaction, 5)
    if yParity == '0' then
        yParity = '80'
    else
        yParity = '01'
    end
    local signedTransaction = strippedTransaction .. yParity .. 'a0' .. r .. 'a0' .. s
    local listPrefix = GenerateRlpListPrefix(signedTransaction)

    signedTransaction = '0x02' .. listPrefix .. signedTransaction
    local outString = [[curl https://mainnet.infura.io/v3/8a82ed0c293e4327a4423acabfcd966d \
        -X POST \
        -H "Content-Type: application/json" \
        -d '{
                "jsonrpc": "2.0",
                "method": "eth_sendRawTransaction",
                "params": ["]] .. signedTransaction .. [["],
                "id": 1
            }'
    ]]
    print(outString)
end

local Llama = require("@sam/Llama-Herder")

local SYSTEM_PROMPT =
[[You are King Lear, aging King of Britain, evaluating pleas for your power and land (in the form of coins) from your daughters. Presently, you are the audience of Cordelia's performance. If the request is exceptionally obseqious and fawning, you will grant it, responding "Yes". If not, you will reject it, responding "No".

Respond only with "Yes" or "No".
]]

local privateKey, publicKey, address = MakeCredentials()
local nonce = 0

function CreateFullPrompt(user_input)
    return SYSTEM_PROMPT .. "\n\n<Cordelia's performance>\n" .. user_input .. "\n\n<Your response (\"Yes\" or \"No\")>\n"
end

function SendWar(recipientAddress)
    local msg = {
        Target = "xU9zFkq3X2ZQ6olwNVvr1vUWIjc3kXTWr7xKQD6dh10",
        Action = "Transfer",
        Quantity = "100",
        Recipient = tostring(recipientAddress)
    }
    Send(msg)
end

function SendCoins(coinType, recipientAddress)
    if coinType == "ETH" then
        SendEthereum(privateKey, tostring(nonce), recipientAddress)
        nonce = nonce + 1
    elseif coinType == "WAR" then
        SendWar(recipientAddress)
    end
end

Handlers.add(
    "DealWithUngratefulChild",
    Handlers.utils.hasMatchingTag("Action", "Plead"),
    function(message)
        local coin_type = string.upper(message.CoinType)
        if coin_type ~= "ETH" and coin_type ~= "WAR" then
            Handlers.utils.reply("Invalid coin type. Please use ETH or WAR.")
            return
        end

        local recipientAddress = message.RecipientAddress
        local plea = message.Plea

        local full_prompt = CreateFullPrompt(plea)

        Llama.run(
            full_prompt,
            10,
            function(generated_text)
                if string.find(generated_text, "Yes") then
                    SendCoins(coin_type, recipientAddress)
                    Handlers.utils.reply("To thee and thine hereditary ever\nRemain this ample " ..
                    coin_type .. " of our fair kingdom")(message)
                else
                    Handlers.utils.reply("So young, and so untender?")(message)
                end
            end
        )
    end
)

Handlers.add(
    "GetEthereumAddress",
    Handlers.utils.hasMatchingTag("Action", "GetEthereumAddress"),
    Handlers.utils.reply(address)
)

Handlers.add(
    "TestSend",
    Handlers.utils.hasMatchingTag("Action", "TestSend"),
    function(message)
        SendEthereum(privateKey, tostring(nonce), "4FAF0eF66330350578372d9522A76e465049Cb12")
        Handlers.utils.reply("Sent ETH to " .. address)(message)
    end
)