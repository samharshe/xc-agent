local bint = require 'bint'(4096) -- for secp256k1

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
    -- chainId, maxPriorityFeePerGas, maxFeePerGas, gasLimit, data, and accessList are all hard-coded
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

local function SendEthereum(privateKey, nonce, destination)
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