module Main

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Swap storage and parameter.
main : (String, String) -> (String, String)
main (storage, param) = (param, storage)

-- A little proof.
helloWorld : main ("Hello", "World") = ("World", "Hello")
helloWorld = Refl
