module Main

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : String -> String
main x = x

-- A really trivial proof.
main_test1 : main "xy" = "xy"
main_test1 = Refl

-- A type-level proof.
main_test2 : (x : String) -> main x = x
main_test2 x = Refl
