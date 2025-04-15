local bint = require('bint')(4096)
local x = bint.fromstring("306890482018661345162541836146332667718")
local y = bint.fromstring("306890482018661345162541836146332667718")

local string_x = bint.tobase(x, 16, true)
local string_y = bint.tobase(y, 16, true)

print(string_x)
print(string_y)

local publicKeyHexString = string.format("%32s", string_x):gsub(" ", "0") .. string.format("%32s", string_y):gsub(" ", "0")
print(publicKeyHexString)