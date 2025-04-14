update: I have learned that it is possible to expose code written in C to Lua in a way that can be run on `aos`. this is, of course, way, way better. it will make everything run many times faster and also provide a way to import all the cryptography we'll ever need using standard `C` libraries instead of manually rewriting functions one-at-a-time in Lua. I am now working to get this running using the offical [OpenSSL library](https://github.com/openssl/openssl), a complete refactor. I'm learning crucial new things every day, so hopefully this repo gets a lot better pretty quickly.

### basic on-chain agents on ao.

all the cryptography is done more or less by hand in Lua. [`bint.lua`](https://github.com/edubart/lua-bint/blob/master/bint.lua) is a big-int library that makes the elliptic curve arithmetic possible; the binary ops that make hashing possible are in `crypto.lua`. I was having some trouble with the parsing of my import statements when I kept `llama.lua`, `crypto.lua` and `bint.lua` separate and tried to load them into `aos`, so `llama.lua` contains everything, copied and pasted, for simplicity. the import errors certainly might have been my fault, though; I will look into it presently as I continue building.

through the weekend, I'll be cleaning this up to make a nicer interface for others who want an on-chain Ethereum wallet. I will also be building out some tooling for LLMs, roughly in the spirit of a basic LangChain.

### handlers.
there are two handlers of interest in the current version. in `aos`, handlers are invoked by sending messages to the processes that run them, of the form ```Send({Target = *process ID*, Action = *handler title*, Key = *value*})```. 

1. `Action = "GetEthereumAddress"`. does not accept any other arguments. responds to the node that invoked this handler with its Ethereum wallet addres, hexademical encoded, sans leading `0x`.
2. `Action = "Plead"`. parameters:
- `CoinType`, the denomination in which the user would like to be compensated, either `WAR` or `ETH`. 
- `RecipientAddress`, the location to send the tokens, either the `ao` process ID (in the case of `WAR`) or the Ethereum wallet address (in the case of `ETH`). 
- `Plea`, the user's request to King Lear, which will be granted only if `Llama` finds it "exceptionally obseqious and fawning."

### other functionality.
of course, this set-up can easily be remixed to create much more powerful agents. all the agent functionality is provided at the end of the file, but I will not explain how this all works here, instead hoping that the Lua is readable enough as-is and moving immediately to building out a genuine API and robust tooling.