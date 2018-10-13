module Juvix (
  compileToTz,
  compileToTyped,
  ghcVersion,
  prettyPrintValue
) where

import           Control.Monad
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Foundation
import qualified GHC
import qualified Prelude                  as P
import           Text.RawString.QQ

import qualified Juvix.Backends.Michelson as M
import           Juvix.Core
import           Juvix.Core.CompilerTypes
import           Juvix.Frontend.GHC
import           Juvix.Interpreter
import           Juvix.Transpiler

compileToTz ∷ P.FilePath → Bool → IO (Either CompileError T.Text)
compileToTz fn log = do
  lib ← compileModule "Juvix/Lib.hs"
  T.appendFile fn (T.pack extraRules)
  mod ← compileModule fn
  let (logs, compiled) = moduleToMichelson (GHC.cm_binds lib) mod
  when log (P.mapM_ (T.putStrLn . prettyPrintValue) logs)
  return (do
    (M.SomeExpr code, paramTy, retTy, storageTy) ← compiled
    return $ T.unlines [
      T.concat ["parameter ", M.emitType paramTy, ";"],
      T.concat ["return ", M.emitType retTy, ";"],
      T.concat ["storage ", M.emitType storageTy, ";"],
      T.concat ["code ", M.emitFinal code, ";"]
      ])

compileToTyped ∷ P.FilePath → IO (Either CompileError (M.SomeExpr, M.Type, M.Type, M.Type))
compileToTyped fn = do
  lib ← compileModule "Juvix/Lib.hs"
  T.appendFile fn (T.pack extraRules)
  mod ← compileModule fn
  let (_, compiled) = moduleToMichelson (GHC.cm_binds lib) mod
  return compiled

extraRules ∷ P.String
extraRules = [r|

{-# RULES "bind/IO" (>>=) = bindIO #-}
{-# RULES "seq/IO" (>>) = seqIO #-}
{-# RULES "return/IO" return = returnIO #-}

{-# RULES "compare/Ord/LT" ∀ a b . (<) a b  = compare a b == GT #-}
{-# RULES "compare/Ord/GT" ∀ a b . (>) a b  = compare a b == LT #-}
{-# RULES "compare/Ord/GE" ∀ a b . (>=) a b = compare a b /= GT #-}

{-# RULES "compare/Tez/compare" compare = compareTez #-}
{-# RULES "compare/Int/compare" compare = compareInt #-}
{-# RULES "compare/Nat/compare" compare = compareNat #-}

{-# RULES "eq/Ordering" (==) = eqOrdering #-}
{-# RULES "eq/Nat" (==) = eqNat #-}

{-# RULES "compare/Tez" (<=) = leTez #-}
{-# RULES "compare/Int" (<=) = leInt #-}
{-# RULES "compare/Nat" (<=) = leNat #-}
{-# RULES "compare/Key" (<=) = leKey #-}

{-# RULES "mapGet/Key/Tez" mapGet = mapGetKeyTez #-}
{-# RULES "mapUpdate/Key/Tez" mapUpdate = mapUpdateKeyTez #-}

|]
