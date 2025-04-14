local Hex = require("hex")

local ROUNDS = 24

local roundConstants = {
    0x0000000000000001,
    0x0000000000008082,
    0x800000000000808A,
    0x8000000080008000,
    0x000000000000808B,
    0x0000000080000001,
    0x8000000080008081,
    0x8000000000008009,
    0x000000000000008A,
    0x0000000000000088,
    0x0000000080008009,
    0x000000008000000A,
    0x000000008000808B,
    0x800000000000008B,
    0x8000000000008089,
    0x8000000000008003,
    0x8000000000008002,
    0x8000000000000080,
    0x000000000000800A,
    0x800000008000000A,
    0x8000000080008081,
    0x8000000000008080,
    0x0000000080000001,
    0x8000000080008008
}

local rotationOffsets = {
    -- ordered for [x][y] dereferencing, so appear flipped here:
    {0, 36, 3, 41, 18},
    {1, 44, 10, 45, 2},
    {62, 6, 43, 15, 61},
    {28, 55, 25, 21, 56},
    {27, 20, 39, 8, 14}
}

local function keccakF(st)
	local permuted = st.permuted
	local parities = st.parities
	for round = 1, ROUNDS do
		-- theta()
		for x = 1,5 do
			parities[x] = 0
			local sx = st[x]
			for y = 1,5 do parities[x] = parities[x] ~ sx[y] end
		end

		local p5, flip, s
		--x=1
		p5 = parities[2]
		flip = parities[5] ~ (p5 << 1 | p5 >> 63)
		s = st[1]
		for y = 1,5 do s[y] = s[y] ~ flip end
		--x=2
		p5 = parities[3]
		flip = parities[1] ~ (p5 << 1 | p5 >> 63)
		s = st[2]
		for y = 1,5 do s[y] = s[y] ~ flip end
		--x=3
		p5 = parities[4]
		flip = parities[2] ~ (p5 << 1 | p5 >> 63)
		s = st[3]
		for y = 1,5 do s[y] = s[y] ~ flip end
		--x=4
		p5 = parities[5]
		flip = parities[3] ~ (p5 << 1 | p5 >> 63)
		s = st[4]
		for y = 1,5 do s[y] = s[y] ~ flip end
		--x=5
		p5 = parities[1]
		flip = parities[4] ~ (p5 << 1 | p5 >> 63)
		s = st[5]
		for y = 1,5 do s[y] = s[y] ~ flip end

		-- rhopi()
		for y = 1,5 do
			local py = permuted[y]
			local r
			for x = 1,5 do
				s, r = st[x][y], rotationOffsets[x][y]
				py[(2*x + 3*y)%5 + 1] = (s << r | s >> (64-r))
			end
		end

		local p, p1, p2
		--x=1
		s, p, p1, p2 = st[1], permuted[1], permuted[2], permuted[3]
		for y = 1,5 do s[y] = p[y] ~ (~ p1[y]) & p2[y] end
		--x=2
		s, p, p1, p2 = st[2], permuted[2], permuted[3], permuted[4]
		for y = 1,5 do s[y] = p[y] ~ (~ p1[y]) & p2[y] end
		--x=3
		s, p, p1, p2 = st[3], permuted[3], permuted[4], permuted[5]
		for y = 1,5 do s[y] = p[y] ~ (~ p1[y]) & p2[y] end
		--x=4
		s, p, p1, p2 = st[4], permuted[4], permuted[5], permuted[1]
		for y = 1,5 do s[y] = p[y] ~ (~ p1[y]) & p2[y] end
		--x=5
		s, p, p1, p2 = st[5], permuted[5], permuted[1], permuted[2]
		for y = 1,5 do s[y] = p[y] ~ (~ p1[y]) & p2[y] end

		-- iota()
		st[1][1] = st[1][1] ~ roundConstants[round]
	end
end


local function absorb(st, inBuffer)

    local hexBuffer = ""
    for i = 1, #inBuffer, 2 do
        local hex = inBuffer:sub(i, i+1)
        local success, result = pcall(function() return tonumber(hex, 16) end)
        if not success then
            error("Invalid hex value: " .. hex)
        end
        hexBuffer = hexBuffer .. string.char(result)
    end

	local blockBytes = st.rate / 8
	local blockWords = blockBytes / 8

	local totalBytes = #hexBuffer + 1

	hexBuffer = hexBuffer .. ( '\x01' .. string.char(0):rep(blockBytes - (totalBytes % blockBytes)))

	totalBytes = #hexBuffer

    local words = {}
    for i = 1, totalBytes - (totalBytes % 8), 8 do
        words[#words + 1] = string.unpack('<I8', hexBuffer, i)
    end

	local totalWords = #words

	-- OR final word with 0x80000000 to set last bit of state to 1
	-- TODO: there's a 1/1088 chance this fails. unfortunately, this must be redone.
	words[totalWords] = words[totalWords] | 0x8000000000000000

	-- XOR blocks into state
	for startBlock = 1, totalWords, blockWords do
		local offset = 0
		for y = 1, 5 do
			for x = 1, 5 do
				if offset < blockWords then
					local index = startBlock+offset
					st[x][y] = st[x][y] ~ words[index]
					offset = offset + 1
				end
			end
		end
		keccakF(st)
	end
end

local function squeeze(st)
	local blockBytes = st.rate / 8
	local blockWords = blockBytes / 4
	local hasht = {}
	local offset = 1
	for y = 1, 5 do
		for x = 1, 5 do
			if offset < blockWords then
				hasht[offset] = string.pack("<I8", st[x][y])
				offset = offset + 1
			end
		end
	end
	return table.concat(hasht)
end

local function keccakHash(data)
	local state = {	{0,0,0,0,0},
					{0,0,0,0,0},
					{0,0,0,0,0},
					{0,0,0,0,0},
					{0,0,0,0,0},
	}
	state.rate = 1088
	local length = 256
	state.permuted = { {}, {}, {}, {}, {}, }
	state.parities = {0,0,0,0,0}
	absorb(state, data)
	local encoded = squeeze(state):sub(1,length/8);

	return Hex.stringToHex(encoded)
end

return keccakHash