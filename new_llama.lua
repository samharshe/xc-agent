local bint = require('bint')(4096)
local keccak = require('Keccak')
local secp256k1 = require('secp256k1')

function GeneratePrivateKey()
    -- TODO
    -- strong intuition that I would get smoked on StackOverflow for this
    -- but we don't have crypto libraries, nor io, and aos's random function doesn't seem to work, so this is what we got
    math.randomseed(math.floor(os.time() * 1000 + os.clock() * 1000))

    local privateKeyTable = {}
    for _ = 1, 32 do
        local randHex = math.random(0, 15)
        table.insert(privateKeyTable, string.format("%x", randHex))
    end

    local result = table.concat(privateKeyTable)

    result = keccak(result)

    return result
end

local function MakeEthereumCreddentials() -- all in one for an Ethereum wallet
    -- initialize public/private key variables
    local privateKeyHexString = nil
    local publicKeyCurvePoint = nil

    -- continue attempting to initialize them until achieving a valid pair
    while not publicKeyCurvePoint do
        privateKeyHexString = GeneratePrivateKey()
        local privateKeyBint = bint.fromstring('0x' .. privateKeyHexString)
        publicKeyCurvePoint = secp256k1.generatePublicKeyCurvePoint(privateKeyBint)
    end
    -- once publicKeyCurvePoint is not nil, ensure that it's smaller than the order of the field
    publicKeyCurvePoint = secp256k1.modulusFieldOrder(publicKeyCurvePoint)

    -- turn curve point into string
    local x, y = table.unpack(publicKeyCurvePoint)
    local hexStringX = bint.tobase(x, 16)
    local hexStringY = bint.tobase(y, 16)
    local publicKeyHexString = string.gsub(string.format("%32s", hexStringX), " ", "0") .. string.gsub(string.format("%32s", hexStringX), " ", "0")
    local publicKeyBint = bint.fromstring('0x'.. publicKeyHexString)
    
    local address = string.sub(keccak(publicKeyBint), -40)

    return privateKeyHexString, publicKeyHexString, address
end

local function SignMessage(privateKey, hash)
    local algorithm = Secp256k1()
    local z = bint.fromstring('0x' .. hash)

    local r = bint.new(0)
    local s = bint.new(0)

    local yParity = ""
    while r:eq(0) or s:eq(0) do
        ::continue::
        local k = bint.fromstring('0x' .. keccak(hash))
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
    local digest = keccak(encodedTransaction)

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