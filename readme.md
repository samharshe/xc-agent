basic on-chain agents on ao.

all the cryptography is done by hand in Lua. `bint.lua` is a big-int library that makes the elliptic curve arithmetic possible; the binary ops that make hashing possible are in `crypto.lua`. I was having some trouble with the parsing of my import statements when I kept `llama.lua`, `crypto.lua` and `bint.lua` separate and tried to load them into `aos`, so `llama.lua` contains everything, copied and pasted, for simplicity. the import errors certainly might have been my fault, though; I will look into it presently as I continue building.

through the weekend, I'll be cleaning this up to make a nicer interface for others who want an on-chain Ethereum wallet. also building out some tooling for LLMs, roughly in the spirit of a basic LangChain.