module Main

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : String -> String
main x = x

-- TODO: Shareholder DAO with ranked-choice voting over mutually exclusive proposals.
