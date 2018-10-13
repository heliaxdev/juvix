module IdrisCG where

import           Foundation
import           IRTS.CodegenCommon

codegenEmpty ∷ CodeGenerator
codegenEmpty ci = do

  putStrLn $ show $ liftDecls ci

  putStrLn "Not implemented"
