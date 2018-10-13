{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Foundation
import           System.Exit

import qualified Juvix       as J

main ∷ IO ()
main = do
  let path = "test.ibc"
  J.transpile path
  exitSuccess
