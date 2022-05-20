module MockIO.T.Process
  ( tests )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.Foldable  ( any )
import Data.Function  ( flip )
import Data.Maybe     ( isJust )
import GHC.Exts       ( IsList( toList ) )

-- bytestring --------------------------

import qualified  Data.ByteString  as  BS
import Data.ByteString  ( ByteString )

-- containers --------------------------

import qualified Data.Set  as  Set

-- containers-plus ---------------------

import ContainersPlus.Insert  ( (⨭) )

-- env-plus ----------------------------

import Env.Types  ( ӭ, ә )

-- exceptions --------------------------

import Control.Monad.Catch ( MonadMask )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile, absfile )

-- lens --------------------------------

import Control.Lens.Fold  ( (^?) )

-- log-plus ----------------------------

import Log           ( Log )
import Log.LogEntry  ( LogEntry, logdoc )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Notice )
                          , discardLogging, runPureLoggingT )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock( NoMock ) )

-- mockio-log --------------------------

import MockIO.Log          ( logit )
import MockIO.MockIOClass  ( MockIOClass )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( IOError, ioeErrorString
                            , ioeFilename, ioeLocation, isNoSuchThingError )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( ProcError )
import MonadIO.Error.ProcExitError    ( ProcExitError, _ProcExitError
                                      , stdErr, stdOut )
import MonadIO.File                   ( devnull )
import MonadIO.Process.CmdSpec        ( CmdArgs( CmdArgs ), CmdExe( CmdExe )
                                      , cmdArgs, cmdExe, expExitVal, mkCmd )
import MonadIO.Process.ExitInfo       ( ExitInfo )
import MonadIO.Process.ExitStatus     ( ExitStatus( ExitVal ), exitVal )
import MonadIO.Process.MakeProc       ( MakeProc )
import MonadIO.Process.OutputHandles  ( OutputHandles )
import MonadIO.Process.ToMaybeTexts   ( ToMaybeTexts )
import MonadIO.Temp                   ( testsWithTempfile )

-- mtl ---------------------------------

import Control.Monad.Reader  ( runReaderT )

-- prettyprinter -----------------------

import Prettyprinter              ( layoutCompact )
import Prettyprinter.Render.Text  ( renderStrict )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, assertBool, assertEqual, assertFailure )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertIOError, assertJust )

-- text --------------------------------

import Data.Text  ( isInfixOf, isSuffixOf, unlines, unpack )

-- text-icu ----------------------------

import qualified  Data.Text.ICU
import Data.Text.ICU  ( Regex, find, regex )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  MockIOPlus.Paths  as  Paths

import MockIO.Process                ( ꙩ, sysN, system )
import MockIO.Process.CmdRW          ( CmdRW( CmdR ) )
import MockIO.Process.OutputDefault  ( OutputDefault )
import MockIO.Process.MLCmdSpec      ( MLCmdSpec )
import MockIO.Process.MLMakeIStream  ( MLMakeIStream )

--------------------------------------------------------------------------------

type 𝔹𝕊 = ByteString

{-| Grep a file using sysN, using different types of output, to test that sysN
    and the differing output types each work.

    We do this by running a simple grep against a temporary file, each time
    checking that the result is as expected (in a given type).
-}

xx ∷ MLCmdSpec ξ → MLCmdSpec ξ
xx x = x & expExitVal ⊢ Set.fromList [1]

sysTests ∷ TestTree
sysTests = testGroup "sysTests" $
  let
    foo ∷ 𝕋
    foo = unlines [ "jimmy 7", "martyn 12", "marbyns 3" ]
    check ∷ (OutputDefault γ, ToMaybeTexts γ, Printable β,
             OutputHandles ζ γ, MakeProc ζ) ⇒
            α → 𝕋 → ((ExitInfo,γ) → Assertion) → (α, β → IO ())

    -- simplifed sysN, with a ProcError error, takes specifically an AbsFile,
    -- 𝕋 args, an MLCmdSpec adjuster, discards logging and never mocks.
    sysN' ∷ ∀ μ ξ ζ .
            (MonadIO μ, OutputDefault ξ, ToMaybeTexts ξ, OutputHandles ζ ξ,
             MakeProc ζ, MonadError ProcError μ) ⇒
            AbsFile → [𝕋] → (MLCmdSpec ξ → MLCmdSpec ξ) → μ (ExitInfo, ξ)
    sysN' p as f =
      discardLogging ∘ flip runReaderT NoMock $ sysN (p, as, f)

--    grp t f = sysN @ProcError (Paths.grep, [t, toText f])
    grp t f = sysN' Paths.grep [t, toText f] id
    -- grep, but expecting an exit 1
    grp' t f = sysN' Paths.grep [t, toText f] xx
--    grp' t f = sysN @ProcError (toMLCmdSpec (Paths.grep, [t, toText f], xx))
    check name t p =
      (name, \ f → ѥ (grp t f) ≫ \ case 𝕽 r → p r
                                        𝕷 e → assertFailure (show e)
      )
    check' name t p =
      (name, \ f → ѥ (grp' t f) ≫ \ case 𝕽 r → p r
                                         𝕷 e → assertFailure (show e)
      )

    assertSuffix t x = assertBool ([fmt|'%t' should be a suffix of '%t'|] t x)
                                  (t `isSuffixOf` x)
    assertBSSuffix t x = assertBool ([fmt|'%w' should be a suffix of '%w'|] t x)
                                    (t `BS.isSuffixOf` x)
  in
    [ testsWithTempfile foo
                        [ check' "()" "xxx"
                                       ((() @=?) ∘ snd)
                        , check "Text" "mar"
                                       ((("martyn 12\nmarbyns 3\n"∷𝕋)@=?) ∘ snd)
                        , check "Bytestring" "mar"
                                       ((("martyn 12\nmarbyns 3\n"∷𝔹𝕊)@=?) ∘snd)
                        , check "[Text]" "mar"
                                (((["martyn 12","marbyns 3"∷𝕋]) @=?) ∘ snd)
                        ]
    , -- use binary input here to generate a stderr msg from grep
      testsWithTempfile ("\x000"⊕foo)
                        [ check "([Text],[Text])" "mar"
                                 (\ (_,(o,[e])) → do
                                     (([]∷[𝕋]) @=? o)
                                     assertSuffix "binary file matches" e
                                 )
                        ]
    , testsWithTempfile ("\x000"⊕foo)
                        [ check "([Text],ByteString)" "mar"
                                 (\ (_,(o,e)) → do
                                     (([]∷[𝕋]) @=? o)
                                     assertBSSuffix "binary file matches\n" e
                                 )
                        ]
    ]
----------------------------------------

grep_ ∷ (MonadIO μ, MLMakeIStream σ,
         MonadError ProcError μ, MonadLog (Log MockIOClass) μ) ⇒
        [𝕋] → σ → μ (ExitInfo, (𝕋,𝕋))
grep_ args input =
  let cmd = mkCmd Paths.grep args & expExitVal ⨭ 1
   in system Notice CmdR (ExitVal 1,("","")) input cmd NoMock

grep ∷ (MonadIO μ, MLMakeIStream σ, MonadError ProcError μ) ⇒
       𝕋 → σ → μ ((ExitInfo, (𝕋,𝕋)), Log MockIOClass)
grep pat input = runPureLoggingT $ grep_ [pat] input

grepaf ∷ (MonadIO μ, MonadError ProcError μ) ⇒
       𝕋 → AbsFile → μ ((ExitInfo, (𝕋,𝕋)), Log MockIOClass)
grepaf pat fn = runPureLoggingT $ devnull ≫ grep_ [pat, toText fn]

-- for repl use

{- | grep a pattern from some `Text`; capture the logs -}
_grep_ ∷ (MonadIO μ, MLMakeIStream σ) ⇒
        𝕋 → σ → μ (𝔼 ProcError ((ExitInfo, (𝕋,𝕋)), Log MockIOClass))
_grep_ pat input = ѥ $ grep pat input

{- | grep a pattern from some `Text`; write the logs to stderr -}

_grep ∷ (MonadIO μ, MonadMask μ, MLMakeIStream σ) ⇒
        𝕋 → σ → μ (𝔼 ProcError (ExitInfo, (𝕋,𝕋)))
_grep pat input = logit $ grep_ [pat] input

{- | Perform a list of tests independently against the result of an IO.
     Note that the IO is performed once for each test. -}
ioTests ∷ ∀ ρ ε . Show ε ⇒
          TestName → IO (𝔼 ε ρ) → [(TestName, ρ → Assertion)] → TestTree
ioTests nm s xs =
  testGroup nm $
    ( \ (n,f ∷ ρ → Assertion) → testCase n $ s ≫ \ x → assertRight f x) ⊳ xs

----------------------------------------

data ProcResult = ProcResult { exit ∷ ExitInfo
                             , out  ∷ 𝕋
                             , err  ∷ 𝕋
                             , log  ∷ Log MockIOClass
                             }

mkProcResult ∷ ((ExitInfo, (𝕋,𝕋)), Log MockIOClass) → ProcResult
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
  ioTests nm s $ [ ("exit",   (\ r → ExitVal expExit ≟ exit r ⊣ exitVal))
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

procTests ∷ TestTree
procTests =
  let
    foo ∷ 𝕋
    foo = unlines [ "jimmy 7", "martyn 12", "marbyns 3" ]
    p   ∷ 𝕋 → IO (𝔼 ProcError ProcResult)
    p t = mkProcResult ⊳⊳ (ѥ @ProcError $ grep t foo)
    logTests ∷ Word8 → [(TestName,[LogEntry MockIOClass] → Assertion)]
    logTests e = [ ([fmt|exit %d|] e, logIncludes ([fmt|Execution exit %d|] e))
                 , ("cmd grep",
                    logMatches $
                      regex [] $ "<CMD> «Pid \\d+» " ⊕ toText Paths.grep)
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

tests ∷ TestTree
tests = testGroup "MonadIO.T.Process" [ procTests, sysTests ]

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests


-- development helpers ---------------------------------------------------------

-- _xx = devnull ≫ (¡ ([absfile|/grep|],[]))
_xx ∷ IO (𝔼 IOError (𝔼 ProcError (ExitInfo, ())))
_xx = logit $  splitMError $ flip runReaderT NoMock $ ꙩ ([absfile|/run/current-system/sw/bin/grep|],["martyn","/etc/group"]::[𝕋],[ӭ (ә "HOME")])

-- that's all, folks! ----------------------------------------------------------
