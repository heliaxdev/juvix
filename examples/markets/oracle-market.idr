module Main

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : String -> String
main x = x

-- TODO: Basic oracle market.
-- Based in part on https://medium.com/numerai/numerai-reveals-erasure-unstoppable-peer-to-peer-data-feeds-4fbb8d92820a
