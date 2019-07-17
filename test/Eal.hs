import           Juvix.Library
import           Juvix.Eal.Eal
import           Juvix.Eal.Parser


parseTest1 = parseEal "!! (λ x : a -o b. !-!- x)"

parseTest2 = parseEal "!!((λ x : Forall. x) (λx : a -o b. (x y)))"

parseTest3 = parseEal "!!(λ x : Forall. !-!-x λx : a -o b -o c. !-!-(x y))"
