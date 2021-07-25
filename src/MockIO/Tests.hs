module MockIO.Tests
  ( tests )
where

-- base --------------------------------

import System.Exit  ( ExitCode )
import System.IO    ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  MockIO.T.Process

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MockIO" [ MockIO.T.Process.tests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------

