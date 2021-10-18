module MockIO.Process
  ( CmdRW(..), (!)
  , doProc, doProc', system, system', systemx, systemx', sys, sysN, sysS )
where

-- base --------------------------------

import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import Data.Word               ( Word8 )
import GHC.Stack               ( HasCallStack )

-- data-default ------------------------

import Data.Default  ( Default )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock, HasDoMock )

-- mockio-log --------------------------

import MockIO.IOClass  ( HasIOClass )
import MockIO.Log      ( MockIOClass, mkIOLMER )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import qualified  MonadIO.Process

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.File                   ( devnull )
import MonadIO.NamedHandle            ( stdin )
import MonadIO.Process                ( throwSig' )
import MonadIO.Process.CmdSpec        ( CmdSpec )
import MonadIO.Process.ExitStatus     ( ExitStatus )
import MonadIO.Process.MakeProc       ( MakeProc )
import MonadIO.Process.OutputHandles  ( OutputHandles )
import MonadIO.Process.ToMaybeTexts   ( ToMaybeTexts )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣) )
import Data.MoreUnicode.Maybe  ( pattern 𝕵 )
import Data.MoreUnicode.Monad  ( (≫) )
import Data.MoreUnicode.Text   ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.Process.CmdRW          ( CmdRW( CmdR, CmdW ), ioc )
import MockIO.Process.MLCmdSpec      ( HasMLCmdSpec( cmdspec, cmdrw, mock
                                                   , mock_value, severity )
                                     , MLCmdSpec
                                     )
import MockIO.Process.MLMakeIStream  ( MLMakeIStream( makeIStream ) )

--------------------------------------------------------------------------------

{- | Execute an external process, wait for termination, return exit status and
     whichever of stderr/stdout were implicitly requested by the return type. -}
system' ∷ ∀ ε ζ ξ σ ω μ .
          (MonadIO μ,
           AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
           Printable ε, MonadError ε μ, HasCallStack,
           ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
           HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
          Severity        -- ^ Severity at which to log action
        → CmdRW           -- ^ Whether this is an 'active' or 'passive' command
        → (ExitStatus, ξ) -- ^ Dummy return values for when mocked
        → σ               -- ^ stdin specification
        → CmdSpec         -- ^ cmd + args
        → DoMock          -- ^ whether to mock this invocation
        → μ (ExitStatus, ξ)

system' sev rw mck_val inh cspec mck = do
  inh' ← makeIStream sev inh mck
  let msg          = [fmtT|<CMD> %T|] cspec
      msgR         ∷ (ExitStatus, ξ) → [𝕋]
      msgR (ex, _) = [[fmtT|exit: %T|] ex]
      doSystem     = MonadIO.Process.system inh' cspec
  mkIOLMER sev (ioc rw) msg (𝕵 msgR) mck_val doSystem mck

--------------------

{- | `system'`, with `ω` specialized to `MockIOClass`. -}
system ∷ ∀ ε ζ ξ σ μ .
         (MonadIO μ,
          AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
          Printable ε, MonadError ε μ, HasCallStack,
          ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
          MonadLog (Log MockIOClass) μ) ⇒
         Severity        -- ^ Severity at which to log action
       → CmdRW           -- ^ Whether this is an 'active' or 'passive' command
       → (ExitStatus, ξ) -- ^ Dummy return values for when mocked
       → σ               -- ^ stdin specification
       → CmdSpec         -- ^ cmd + args
       → DoMock          -- ^ whether to mock this invocation
       → μ (ExitStatus, ξ)
system = system'

----------------------------------------

{- | Like `system`, but does not throw on any process exit/signal. -}
systemx' ∷ ∀ ε ζ ξ σ ω μ .
           (MonadIO μ,
            AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
            Printable ε, MonadError ε μ, HasCallStack,
            ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
            HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
           Severity        -- ^ Severity at which to log action
         → CmdRW           -- ^ Whether this is an 'active' or 'passive' command
         → (ExitStatus, ξ) -- ^ Dummy return values for when mocked
         → σ               -- ^ stdin specification
         → CmdSpec         -- ^ cmd + args
         → DoMock          -- ^ whether to mock this invocation
         → μ (ExitStatus, ξ)

systemx' sev rw mck_val inh cspec mck = do
  inh' ← makeIStream sev inh mck
  let msg          = [fmtT|<CMD> %T|] cspec
      msgR         ∷ (ExitStatus, ξ) → [𝕋]
      msgR (ex, _) = [[fmtT|exit: %T|] ex]
      doSystem     = MonadIO.Process.systemx inh' cspec
  mkIOLMER sev (ioc rw) msg (𝕵 msgR) mck_val doSystem mck

--------------------

{- | `systemx`, with `ω` specialized to `MockIOClass`. -}
systemx ∷ ∀ ε ζ ξ σ μ .
           (MonadIO μ,
            AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
            Printable ε, MonadError ε μ, HasCallStack,
            ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
            MonadLog (Log MockIOClass) μ) ⇒
           Severity        -- ^ Severity at which to log action
         → CmdRW           -- ^ Whether this is an 'active' or 'passive' command
         → (ExitStatus, ξ) -- ^ Dummy return values for when mocked
         → σ               -- ^ stdin specification
         → CmdSpec         -- ^ cmd + args
         → DoMock          -- ^ whether to mock this invocation
         → μ (ExitStatus, ξ)

systemx = systemx'

{-
λ> :m + Control.Monad.Log MonadIO.Process.ExitStatus FPath.AbsFile
        MockIO.OpenFile MonadIO.Process.CmdSpec MockIO MonadError Data.Function
        MonadIO.Error.CreateProcError Data.Either MockIO.Log
λ> let cspec = mkCmd [absfile|/run/current-system/sw/bin/grep|]
                     ["martyn","/etc/group"]
λ> logit' $ splitMError @ProcError @(Either _)
          $ system Notice CmdR (ExitVal 0, ()) [absfile|/dev/null|] cspec NoMock
-}

{- | Like `system`, but collects all of the cmd specification (including mock /
     log bits) into MLCmdSpec. -}
sys' ∷ ∀ ε ζ ξ σ ω μ .
       (MonadIO μ,
        AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
        Printable ε, MonadError ε μ, HasCallStack,
        ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
        HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
       σ → MLCmdSpec ξ → μ (ExitStatus, ξ)
sys' inh mlc = system' (mlc ⊣ severity) (mlc ⊣ cmdrw) (mlc ⊣ mock_value) inh
                       (mlc ⊣ cmdspec) (mlc ⊣ mock)

sys ∷ ∀ ε ζ ξ σ μ .
      (MonadIO μ,
       AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
       Printable ε, MonadError ε μ, HasCallStack,
       ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
       MonadLog (Log MockIOClass) μ) ⇒
      σ → MLCmdSpec ξ → μ (ExitStatus, ξ)
sys = sys'

--------------------

{- | Like `sys`, but implicitly takes `/dev/null` for input. -}
sysN ∷ ∀ ε ζ ξ μ .
       (MonadIO μ,
        AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
        Printable ε, MonadError ε μ, HasCallStack,
        ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ,
        MonadLog (Log MockIOClass) μ) ⇒
       MLCmdSpec ξ → μ (ExitStatus, ξ)
sysN c = devnull ≫ \ l → sys l c

sysS ∷ ∀ ε ζ ξ μ .
       (MonadIO μ,
        AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
        Printable ε, MonadError ε μ, HasCallStack,
        ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ,
        MonadLog (Log MockIOClass) μ) ⇒
       MLCmdSpec ξ → μ (ExitStatus, ξ)
sysS = sys stdin

{-

λ> :m + FPath.AbsFile MonadIO.Error.CreateProcError Data.Either Data.Text
        Control.Monad.Log MockIO MonadError Data.Function MockIO.Log

λ> let mlc = mkMLCmd Notice CmdR [absfile|/run/current-system/sw/bin/grep|]
                                 ["martyn","/etc/group"] NoMock

λ> logit' $ splitMError @ProcError @(Either _)
          $ sys @_ @_ @Text [absfile|/dev/null|] mlc

------------------------------------------------------------

λ> :m + FPath.AbsFile MonadIO.Error.CreateProcError Data.Either Data.Text
        Control.Monad.Log MockIO MonadError Data.Function MockIO.Log
        MockIO.Process.MLCmdSpec

λ> let mlc = mkMLCmdR [absfile|/run/current-system/sw/bin/grep|]
                      ["martyn","/etc/group"] NoMock & expExitVal <& 1

λ> Right (Right (ev,(t::Text))) <- logit' $ splitMError @ProcError @(Either _)
                                          $ [absfile|/dev/null|] ! mlc

λ> Data.Text.IO.putStrLn t

-}

(!) ∷ ∀ ε ζ ξ σ μ .
      (MonadIO μ,
       AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
       Printable ε, MonadError ε μ, HasCallStack,
       ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
       MonadLog (Log MockIOClass) μ) ⇒
      σ → MLCmdSpec ξ → μ (ExitStatus, ξ)
(!) = sys

----------------------------------------

doProc' ∷ ∀ ε ζ ξ σ ω μ .
         (MonadIO μ, MLMakeIStream σ,
          ToMaybeTexts ξ, MakeProc ζ, OutputHandles ζ ξ,
          AsProcExitError ε, AsCreateProcError ε, AsFPathError ε,
          AsIOError ε, Printable ε, MonadError ε μ,
          HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
         μ () → Severity → CmdRW → (ExitStatus, ξ) → σ → CmdSpec → DoMock
       → μ (Word8, ξ)
doProc' finally sev rw mck_val input cspec mck = do
  result ← ѥ $ systemx' sev rw mck_val input cspec mck
  finally
  throwSig' cspec result

----------------------------------------

doProc ∷ ∀ ε ζ ξ σ μ .
        (MonadIO μ, MLMakeIStream σ,
         ToMaybeTexts ξ, MakeProc ζ, OutputHandles ζ ξ,
         AsProcExitError ε, AsCreateProcError ε, AsFPathError ε, AsIOError ε,
         Printable ε, MonadError ε μ, MonadLog (Log MockIOClass) μ) ⇒
        μ () → Severity → CmdRW → (ExitStatus, ξ) → σ → CmdSpec → DoMock
      → μ (Word8, ξ)

doProc = doProc'

-- that's all, folks! ----------------------------------------------------------
