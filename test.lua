local secp256k1 = require('secp256k1')
local bint = require('bint')(4096)

local privateKeyHexString = "fe9239ef21ee6dac635793fcdd2a98564942a69247aa70760b076f572fd6d4fb"
local privateKeyBint = bint.fromstring('0x' .. privateKeyHexString)
local publicKeyCurvePoint = secp256k1.generatePublicKeyCurvePoint(privateKeyBint)
local x, y = table.unpack(publicKeyCurvePoint)
local hexStringX = bint.tobase(x, 16)
local hexStringY = bint.tobase(y, 16)
local publicKeyHexString = string.gsub(string.format("%32s", hexStringX), " ", "0") .. string.gsub(string.format("%32s", hexStringY), " ", "0")

print('privateKeyHexString: ' .. tostring(privateKeyHexString))
print('hexStringX: ' .. hexStringX)
print('hexStringY: ' .. hexStringY)
print('publicKeyHexString: ' .. tostring(publicKeyHexString))