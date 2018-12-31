module Main

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : String -> String
main x = x

-- TODO: Basic prediction market.
-- Base it on this whitepaper: http://www.truthcoin.info/papers/truthcoin-whitepaper.pdf
