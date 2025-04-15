-- secp256k1 implementation nearly completely from https://github.com/andreacorbellini/ecc, which and whose accompanying blog series are A+
local bint = require('bint')(4096)

CURVE_CONSTANTS = {
    p = bint.fromstring("0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f"),
    a = bint.fromstring("0x0"),
    b = bint.fromstring("0x7"),
    g = {
        bint.fromstring("0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
        bint.fromstring("0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8")
    },
    n = bint.fromstring("0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141"),
    h = bint.fromstring("0x1")
}

function isPointOnCurve(point)
    local x, y = table.unpack(point)

    local val = (((y * y) - (x * x * x) - (CURVE_CONSTANTS.a * x) - CURVE_CONSTANTS.b) % CURVE_CONSTANTS.p) -- just the curve equation
    return val == 0
end

function CurveNegate(algorithm, point)
    if point == nil then
        return nil
    end

    local x, y = table.unpack(point)
    local newY = -y % CURVE_CONSTANTS.p
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

local function multiply(scalar, curvePoint)
    if scalar < 0 then
        return multiply(-scalar, CurveNegate(curvePoint))
    end

    local result = nil
    local addend = curvePoint

    -- double and add
    scalar = tostring(scalar)
    scalar = DecimalToBinary(scalar)
    while scalar ~= "0" and scalar ~= "" do
        if scalar:sub(-1) == "1" then
            result = CurveAdd(result, addend)
        end

        addend = CurveAdd(addend, addend)

        scalar = scalar:sub(1, -2)
    end

    return result
end

local function generatePublicKeyCurvePoint(privateKey)
    local publicKeyCurvePoint = nil
    publicKeyCurvePoint = multiply(CURVE_CONSTANTS.g, privateKey)

    local x, y = table.unpack(publicKeyCurvePoint)
    publicKeyCurvePoint = {x % CURVE_CONSTANTS.p, y % CURVE_CONSTANTS.P}

    return publicKeyCurvePoint
end

local function modulusFieldOrder(curvePoint)
    local x, y = table.unpack(curvePoint)

    return {x % CURVE_CONSTANTS.p, y % CURVE_CONSTANTS.p}
end


return {
    generatePublicKeyCurvePoint = generatePublicKeyCurvePoint,
    modulusFieldOrder = modulusFieldOrder,
}