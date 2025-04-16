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

-- local function isPointOnCurve(point)
--     local x, y = table.unpack(point)

--     -- simply check whether the point satisfies the curve equation
--     return (((y * y) - (x * x * x) - (CURVE_CONSTANTS.a * x) - CURVE_CONSTANTS.b) % CURVE_CONSTANTS.p):eq(0)
-- end

local function inverseMod(k, p)
    if k:eq(0) then
        error('division by zero')
    end

    if k < 0 then
        return p - inverseMod(-k, p)
    end

    local s, old_s = 0, 1
    local t, old_t = 1, 0
    local r, old_r = p, k

    while not bint.eq(r, 0) do
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

local function modulusFieldOrder(curvePoint)
    local x, y = table.unpack(curvePoint)
    x = x % CURVE_CONSTANTS.p
    y = y % CURVE_CONSTANTS.p

    return {x, y}
end

local function curveAdd(curvePoint1, curvePoint2)
    if curvePoint1 == nil then
        return curvePoint2
    end
    if curvePoint2 == nil then
        return curvePoint1
    end

    local x1, y1 = table.unpack(curvePoint1)
    local x2, y2 = table.unpack(curvePoint2)

    local m = nil
    if x1:eq(x2) then
        if not y1:eq(y2) then
            return nil
        else
            m = (3 * x1 * x1 + CURVE_CONSTANTS.a) * inverseMod(2 * y1, CURVE_CONSTANTS.p)
        end
    else
        m = (y1 - y2) * inverseMod(x1 - x2, CURVE_CONSTANTS.p)
    end

    local x3 = m * m - x1 - x2
    local y3 = y1 + m * (x3 - x1)
    print('x3: ' .. tostring(x3))
    print('y3: ' .. tostring(y3))
    local result = modulusFieldOrder({x3, y3})
    print(result[1])
    print(result[2])

    return result
end

local function curveNegate(curvePoint)
    local x, y = table.unpack(curvePoint)
    return {x, -y}
end

local function curveMultiply(scalar, curvePoint)
    if scalar < 0 then
        return curveMultiply(-scalar, curveNegate(curvePoint))
    end

    local result = nil
    local addend = curvePoint

    -- double and add, taking advantage of bint operations
    while not scalar:eq(0) do
        if bint.isodd(scalar) then
            result = curveAdd(result, addend)
        end
        addend = curveAdd(addend, addend)
        print(tostring(scalar))
        print(bint.tobase(scalar, 2))
        scalar = scalar >> 1
    end

    return result
end

local function generatePublicKeyCurvePoint(privateKey)
    local publicKeyCurvePoint = nil
    publicKeyCurvePoint = curveMultiply(privateKey, CURVE_CONSTANTS.g)

    return publicKeyCurvePoint
end

return {
    generatePublicKeyCurvePoint = generatePublicKeyCurvePoint,
    modulusFieldOrder = modulusFieldOrder,
    curveMultiply = curveMultiply
}