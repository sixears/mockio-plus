module MockIO.T.Process
  ( tests )
where

-- base --------------------------------

import Data.Either    ( Either )
import Data.Eq        ( Eq )
import Data.Foldable  ( any )
import Data.Function  ( ($), (&) )
import Data.Maybe     ( isJust )
import Data.Word      ( Word8 )
import GHC.Exts       ( IsList( toList ) )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers-plus ---------------------

import ContainersPlus.Insert  ( (⨭) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- exceptions --------------------------

import Control.Monad.Catch ( MonadMask )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile, absfile )

-- lens --------------------------------

import Control.Lens.Fold  ( (^?) )
import Control.Lens.Lens  ( Lens' )

-- log-plus ----------------------------

import Log           ( Log )
import Log.LogEntry  ( LogEntry, logdoc )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Notice )
                          , runPureLoggingT )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock( NoMock ) )

-- mockio-log --------------------------

import MockIO.Log          ( logit )
import MockIO.MockIOClass  ( MockIOClass )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( AsIOError( _IOError ), ioeErrorString
                            , ioeFilename, ioeLocation, isNoSuchThingError )

-- monadio-plus ------------------------

import MonadIO                        ( MonadIO )
import MonadIO.Error.CreateProcError  ( ProcError )
import MonadIO.Error.ProcExitError    ( ProcExitError, _ProcExitError
                                      , stdErr, stdOut )
import MonadIO.File                   ( devnull )
import MonadIO.Process.CmdSpec        ( CmdArgs( CmdArgs ), CmdExe( CmdExe )
                                      , cmdArgs, cmdExe, expExitVal, mkCmd )
import MonadIO.Process.ExitStatus     ( ExitStatus( ExitVal ), exitVal )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( pattern 𝕿 )
import Data.MoreUnicode.Either   ( 𝔼 )
import Data.MoreUnicode.Functor  ( (⊳), (⊳⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Maybe    ( pattern 𝕵 )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- prettyprinter -----------------------

import Prettyprinter              ( layoutCompact )
import Prettyprinter.Render.Text  ( renderStrict )

-- tasty -------------------------------

import Test.Tasty  ( TestName, TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, (@=?), assertBool, assertEqual, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertIOError, assertJust, assertRight, runTestsP
                  , runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( isInfixOf, unlines, unpack )

-- text-icu ----------------------------

import qualified  Data.Text.ICU
import Data.Text.ICU  ( Regex, find, regex )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  MockIOPlus.Paths  as  Paths

import MockIO.Process                ( system )
import MockIO.Process.CmdRW          ( CmdRW( CmdR ) )
import MockIO.Process.MLMakeIStream  ( MLMakeIStream )

--------------------------------------------------------------------------------

foo ∷ 𝕋
foo = unlines [ "jimmy 7"
              , "martyn 12"
              , "marbyns 3"
              ]

grep_ ∷ (MonadIO μ, MLMakeIStream σ,
         MonadError ProcError μ, MonadLog (Log MockIOClass) μ) ⇒
        [𝕋] → σ → μ (ExitStatus, (𝕋,𝕋))
grep_ args input =
  let cmd = mkCmd Paths.grep args & expExitVal ⨭ 1
   in system Notice CmdR (ExitVal 1,("","")) input cmd NoMock

grep ∷ (MonadIO μ, MLMakeIStream σ, MonadError ProcError μ) ⇒
       𝕋 → σ → μ ((ExitStatus, (𝕋,𝕋)), Log MockIOClass)
grep pat input = runPureLoggingT $ grep_ [pat] input

grepaf ∷ (MonadIO μ, MonadError ProcError μ) ⇒
       𝕋 → AbsFile → μ ((ExitStatus, (𝕋,𝕋)), Log MockIOClass)
grepaf pat fn = runPureLoggingT $ devnull ≫ grep_ [pat, toText fn]

-- for repl use

{- | grep a pattern from some `Text`; capture the logs -}
_grep_ ∷ (MonadIO μ, MLMakeIStream σ) ⇒
        𝕋 → σ → μ (𝔼 ProcError ((ExitStatus, (𝕋,𝕋)), Log MockIOClass))
_grep_ pat input = ѥ @_ @(Either _) $ grep pat input

{- | grep a pattern from some `Text`; write the logs to stderr -}

_grep ∷ (MonadIO μ, MonadMask μ, MLMakeIStream σ) ⇒
        𝕋 → σ → μ (𝔼 ProcError (ExitStatus, (𝕋,𝕋)))
_grep pat input = logit $ grep_ [pat] input

{- | Perform a list of tests independently against the result of an IO.
     Note that the IO is performed once for each test. -}
ioTests ∷ ∀ ρ ε . Show ε ⇒
          TestName → IO (𝔼 ε ρ) → [(TestName, ρ → Assertion)] → TestTree
ioTests nm s xs =
  testGroup nm $
    ( \ (n,f ∷ ρ → Assertion) → testCase n $ s ≫ \ x → assertRight f x) ⊳ xs

----------------------------------------

data ProcResult = ProcResult { exit ∷ ExitStatus
                             , out  ∷ 𝕋
                             , err  ∷ 𝕋
                             , log  ∷ Log MockIOClass
                             }

mkProcResult ∷ ((ExitStatus, (𝕋,𝕋)), Log MockIOClass) → ProcResult
mkProcResult ((ex,(ot,er)),lg) = ProcResult ex ot er lg

{- | Test the results of an external process.  Note that the proc is run
     multiple times (once for each test). -}
testProc ∷ TestName
         → IO (𝔼 ProcError ProcResult)
         → Word8                                -- ^ expected exit
         → 𝕋                                    -- ^ expected stdout
         → 𝕋                                    -- ^ expected stderr
           -- | tests against the log output
         → [(TestName,[LogEntry MockIOClass] → Assertion)]
         → TestTree
testProc nm s expExit expOut expErr expLog =
  ioTests nm s $ [ ("exit",   (\ r → ExitVal expExit ≟ exit r))
                 , ("stdout", (\ r → expOut  ≟ out r))
                 , ("stderr", (\ r → expErr  ≟ err r))
                 ]
               ⊕ [ ("log: " ⊕ n, (\ r → p (toList $ log r))) | (n,p) ← expLog ]

----------------------------------------

{- A single log entry rendered as (strict) Text. -}
logEntryText ∷ LogEntry ρ → 𝕋
logEntryText logEntry = renderStrict (layoutCompact (logEntry ⊣ logdoc))

----------------------------------------

{- Do a list of log entries include one with the given text? -}
logIncludes ∷ 𝕋 → [LogEntry ρ] → Assertion
logIncludes t ls = 𝕿 @=? any (\ entry → t `isInfixOf` logEntryText entry) ls

----------------------------------------

{- Do a list of log entries include one with the given text? -}
logMatches ∷ Regex → [LogEntry ρ] → Assertion
logMatches r ls =
  assertBool (unpack $ Data.Text.ICU.pattern r) $
    any (\ entry → isJust $ r `find` logEntryText entry) ls

----------------------------------------

tests ∷ TestTree
tests =
  let
    p ∷ 𝕋 → IO (𝔼 ProcError ProcResult)
    p t = mkProcResult ⊳⊳ (ѥ @ProcError $ grep t foo)
    logTests ∷ Word8 → [(TestName,[LogEntry MockIOClass] → Assertion)]
    logTests e = [ ([fmt|exit %d|] e, logIncludes ([fmt|Execution exit %d|] e))
               , ("cmd grep", logIncludes $ [fmt|<CMD> %T|] Paths.grep)
               , ("provided text",
                  logMatches $
                    regex [] "using provided Text «jimmy.*» as input stream")
               ]
  in
    testGroup
      "Process"
      [ -- grep matches 'martyn', exits 0
        testProc "grep martyn" (p "martyn") 0 "martyn 12\n" "" (logTests 0)
      , -- grep matches nothing, exits 1
        testProc "grep john"   (p "john")   1 "" "" (logTests 1)
      , testGroup "missing file" $ -- grep exits 2 due to missing file
          let
            -- grepaf passes the file as an argument to grep
            grepf = grepaf "x" [absfile|/nonesuch|]
            testErrs ∷ (Eq α, Show α) ⇒
                       𝕊 → (Lens' ProcExitError α) → α → Assertion
            testErrs n f x =
              assertIOError
                (\ e → assertEqual n (𝕵 x) (e ^? _ProcExitError ∘ f)) grepf
            testE ∷ (Eq α, Printable α, Show α) ⇒
                      (Lens' ProcExitError α) → α → Assertion
            testE f x =
              testErrs (toString x) f x
            testErr' ∷ (Eq α, Printable α, Show α) ⇒
                       (Lens' ProcExitError α) → α → TestTree
            testErr' f x =
              testCase (toString x) $ testE f x
          in
            [
              let
                isProcExitError e =
                  assertBool (toString e) $ isJust (e ^? _ProcExitError)
               in
                testCase "ProcExitError" $ assertIOError isProcExitError grepf
            , testErr' exitVal (ExitVal 2)
            , testErr' cmdExe (CmdExe Paths.grep)
            , testErr' cmdArgs (CmdArgs ["x","/nonesuch"])
            , testCase "stdout" $ testErrs "stdout" stdOut (𝕵 "")
            , let
                msg = [fmt|%T: /nonesuch: No such file or directory\n|]
                      Paths.grep
               in
                testCase "stderr" $ testErrs "stderr" stdErr (𝕵 msg)
            ]
      , testGroup "missing input stream file" $
          -- fail to open input stream; IOErr
          let
            -- grep opens the file to pass on grep's stdin
            grepz = grep "x" [absfile|/nonesuch|]
          in
            [
              let
                isIOError e =
                  assertBool (toString e) $ isJust (e ^? _IOError)
               in
                testCase "IOError" $ assertIOError isIOError grepz
            , let
                assertIsNoSuchThing =
                  assertBool "no such thing" ∘ isNoSuchThingError
               in
                testCase "file not found" $
                  assertIOError assertIsNoSuchThing grepz
            , testCase "does not exist" $
                assertIOError (assertJust (≟ "/nonesuch") ∘ ioeFilename) grepz
            , let
                exp = "does not exist"
              in
                testCase "openFd" $
                  assertIOError (assertJust (≟ exp) ∘ ioeErrorString) grepz
            , testCase "file not found" $
                assertIOError (assertJust (≟ "openFd") ∘ ioeLocation) grepz
            ]
      ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
