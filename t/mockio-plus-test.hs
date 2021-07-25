{-# LANGUAGE UnicodeSyntax #-}

-- tasty -------------------------------

import Test.Tasty           ( defaultIngredients )
import Test.Tasty.Runners   ( defaultMainWithIngredients )

--------------------------------------------------------------------------------

-- if they compile, they work...

import MockIO.Directory             ( )
import MockIO.File                  ( )
-- imported by MockIO.File
--   import MockIO.OpenFile              ( )
-- imported by MockIO.T.Process
--   import MockIO.Process               ( )
--   import MockIO.Process.CmdRW         ( )
-- imported by MockIO.Process
--   import MockIO.Process.MLCmdSpec     ( )
-- imported by MockIO.T.Process
--   import MockIO.Process.MLMakeIStream ( )
-- imported by MockIO.Process.MLCmdSpec     ( )
--   import MockIO.Process.OutputDefault ( )
-- imported by MockIO.Tests
--   import MockIO.T.Process             ( )
-- imported for tests
--   import MockIO.Tests                 ( )
-- imported by MockIO.T.Process
--   import MockIOPlus.Paths             ( )

-- we actually have tests, too!

import MockIO.Tests  ( tests )

main ∷ IO ()
main = defaultMainWithIngredients defaultIngredients tests
