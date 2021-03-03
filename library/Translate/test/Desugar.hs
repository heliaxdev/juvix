module Desugar where

import qualified Juvix.Desugar.Types as AST
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.FrontendDesugar as Desugar
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSym
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Text.Megaparsec as P

allDesugar :: T.TestTree
allDesugar =
  T.testGroup
    "desugar Tests"
    [guardTest]

shouldDesugar :: T.TestName -> ByteString -> [AST.TopLevel] -> T.TestTree
shouldDesugar name x y =
  T.testGroup
    "Desugar tests"
    [ T.testCase
        ("desugar: " <> name)
        ( fmap Desugar.op (P.parse (many Parser.topLevelSN) "" x)
            T.@=? Right y
        )
    ]

guardTest :: T.TestTree
guardTest =
  shouldDesugar
    "guardTest"
    "let foo | x == 3 = 3 | else = 2"
    [ ( AST.MatchL
          { matchLPattern =
              AST.MatchCon "True" []
                |> flip AST.MatchLogic Nothing,
            matchLBody =
              AST.Constant (AST.Number (AST.Integer' 3))
          }
          :| [ ( AST.MatchL
                   { matchLPattern =
                       AST.MatchCon "True" []
                         |> flip AST.MatchLogic Nothing,
                     matchLBody =
                       AST.Constant (AST.Number (AST.Integer' 2))
                   }
                   :| []
               )
                 |> AST.Match'' (AST.Name "else")
                 |> AST.Match
                 |> AST.MatchL
                   (AST.MatchLogic (AST.MatchCon "False" []) Nothing)
             ]
      )
        |> AST.Match''
          ( AST.Integer' 3
              |> AST.Number
              |> AST.Constant
              |> AST.Inf (AST.Name "x") "=="
              |> AST.Infix
          )
        |> AST.Match
        |> AST.Like []
        |> (\x -> AST.Func "foo" (pure x) Nothing)
        |> AST.Function
    ]

handlerTest :: T.TestTree
handlerTest =
  shouldDesugar
    "handlerTest"
    "handler pure x = x"
     [ AST.Name "x"
       |> AST.Body
       |> AST.Like "pure"
       [
         AST.MatchCon "x"
         |> flip AST.MatchLogic Nothing
         |> AST.ConcreteA
       ]
       |> AST.Func
       |> AST.Function
      ]

-- Right [ Function' (FunctionX ("pure",
--                              FunctionLikeX {
--                                 extFunctionLike = ([ConcreteA' (
--                                                        MatchLogic'
--                                                          { matchLogicContents = MatchName' "x" ()
--                                                          , matchLogicNamed = Nothing, annMatchLogic = ()}) ()]
--                                                   , Name' ("x" :| []) ())
--                                            } :| [],Nothing))
--         ()
--       ]

viaTest :: T.TestTree
viaTest =
  shouldDesugar
    "viaTest"
    "let foo = a via b"
    [ (AST.Name "a" :| AST.Name "b" :| [])
      |> AST.App (AST.Name "x")
      |> AST.Application
      |> AST.Body
      |> AST.Like "foo" []
      |> AST.Func
      |> AST.Function
    ]
-- Right [ Function' (FunctionX ("foo",
--                               FunctionLikeX {
--                                  extFunctionLike = ([],
--                                                     Application' (App' {
--                                                                      applicationName = Name' ("b" :| []) ()
--                                                                      , applicationArgs = Name' ("a" :| []) () :| [], annApp = ()
--                                                                      }) ())
--                                  } :| []
--                              , Nothing))
--         ()
--       ]
