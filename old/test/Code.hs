module Code where

import           Foundation

import           Code.Basic
import           Code.Examples
import           Code.Features
import           Code.MichelsonLang
import           Code.SideEffects
import           Types

transpilationTestCases ∷ [TranspilationTestCase]
transpilationTestCases = mconcat [basicTranspilationTestCases, featuresTranspilationTestCases, sideEffectsTranspilationTestCases, michelsonLangTranspilationTestCases, examplesTranspilationTestCases]
