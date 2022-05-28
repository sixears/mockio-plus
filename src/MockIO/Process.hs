module MockIO.Process
  ( CmdRW(..), (¡), ꙩ, ꙫ
  , doProc, doProc', system, system', systemx, systemx', sys, sys', sysN, sysS )
where

import Base1T hiding ( toList )

-- base --------------------------------

import Data.Function  ( flip )
import Data.List      ( sortOn )
import Data.Maybe     ( fromMaybe )
import GHC.Exts       ( IsList( toList ) )

-- env-plus ----------------------------

import Env        ( getEnvironment )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log, logIO )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock( DoMock ), HasDoMock )

-- mockio-log --------------------------

import MockIO.IOClass  ( HasIOClass( ioClass ) )
import MockIO.Log      ( MockIOClass, doMock, logResult )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.File                   ( devnull )
import MonadIO.NamedHandle            ( stdin )
import MonadIO.Process                ( getPid', procWait, throwExit, throwSig')
import MonadIO.Process.CmdSpec        ( CmdSpec, env )
import MonadIO.Process.ExitInfo       ( ExitInfo, exitInfo )
import MonadIO.Process.ExitStatus     ( ExitStatus, exitVal )
import MonadIO.Process.MakeProc       ( MakeProc, makeProc )
import MonadIO.Process.OutputHandles  ( OutputHandles )
import MonadIO.Process.Pid            ( Pid( Pid ) )
import MonadIO.Process.ToMaybeTexts   ( ToMaybeTexts )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks )

-- safe --------------------------------

import Safe  ( succSafe )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.Process.CmdRW          ( CmdRW( CmdR, CmdW ), ioc )
import MockIO.Process.MLCmdSpec      ( HasMLCmdSpec( cmdspec, cmdrw, mock
                                                   , mock_value, severity )
                                     , ToCmdSpec( toCmdSpec )
                                     , ToMLCmdSpec( toMLCmdSpec )
                                     )
import MockIO.Process.MLMakeIStream  ( MLMakeIStream( makeIStream ) )
import MockIO.Process.OutputDefault  ( OutputDefault )

--------------------------------------------------------------------------------

{- | Execute an external process, wait for termination, return exit status and
     whichever of stderr/stdout were implicitly requested by the return type. -}
system' ∷ ∀ ε ζ ξ σ ω χ μ .
          (MonadIO μ, ToCmdSpec χ, HasCallStack,
           AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
           Printable ε, MonadError ε μ, HasCallStack,
           ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
           HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
          Severity        -- ^ Severity at which to log action
        → CmdRW           -- ^ Whether this is an 'active' or 'passive' command
        → (ExitStatus, ξ) -- ^ Dummy return values for when mocked
        → σ               -- ^ stdin specification
        → χ               -- ^ cmd + args
        → DoMock          -- ^ whether to mock this invocation
        → μ (ExitInfo, ξ)

system' sev rw (mck_estat,mck_res) inh cspec mck =
  systemx' sev rw (mck_estat,mck_res) inh cspec mck ≫ throwExit

--------------------

{- | `system'`, with `ω` specialized to `MockIOClass`. -}
system ∷ ∀ ε ζ ξ σ χ μ .
         (MonadIO μ, ToCmdSpec χ, HasCallStack,
          AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
          Printable ε, MonadError ε μ, HasCallStack,
          ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
          MonadLog (Log MockIOClass) μ) ⇒
         Severity        -- ^ Severity at which to log action
       → CmdRW           -- ^ Whether this is an 'active' or 'passive' command
       → (ExitStatus, ξ) -- ^ Dummy return values for when mocked
       → σ               -- ^ stdin specification
       → χ               -- ^ cmd + args
       → DoMock          -- ^ whether to mock this invocation
       → μ (ExitInfo, ξ)
system = system'

----------------------------------------

{- | Like `system`, but does not throw on any process exit/signal. -}
systemx' ∷ ∀ ε ζ ξ σ ω χ μ .
           (MonadIO μ, ToCmdSpec χ, HasCallStack,
            AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
            Printable ε, MonadError ε μ, HasCallStack,
            ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
            HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
           Severity        -- ^ Severity at which to log action
         → CmdRW           -- ^ Whether this is an 'active' or 'passive' command
         → (ExitStatus, ξ) -- ^ Dummy return values for when mocked
         → σ               -- ^ stdin specification
         → χ               -- ^ cmd + args
         → DoMock          -- ^ whether to mock this invocation
         → μ (ExitInfo, ξ)

systemx' sev rw (mck_estat, mck_res) inh cspec mck = do
  inh'                     ← makeIStream sev inh mck
  (cmd_spec, env_mod_msgs) ← toCmdSpec cspec
  let mck_val       = (exitInfo mck_estat cmd_spec (Pid 0), mck_res)
      io_class      = def & ioClass ⊢ (ioc rw) & doMock ⊢ mck
      pp            ∷ 𝕋 → 𝕋
      pp t          = if mck ≡ DoMock then "(" ⊕ t ⊕ ")" else t
      penv ∷ (MonadIO ν ,MonadLog (Log ω) ν) ⇒ Pid → CmdSpec → ν ()
      penv p c    = do
        logIO sev io_class $ pp $ [fmt|<CMD> «%w» %T|] p cmd_spec
        e ← (flip fromMaybe (c ⊣ env)) ⊳ getEnvironment
        forM_ env_mod_msgs $ \ t →
          logIO (succSafe sev) io_class $ pp $ [fmt|<ENVMOD> «%w» %t|] p t
        forM_ (sortOn fst $ toList e) $ \ (ek,ev) →
          logIO (succSafe $ succSafe sev) io_class $
            pp $ [fmt|<ENV> «%w» %q → %q|] p ek ev

  (result,p) ← if DoMock ≡ mck
               then do penv (Pid 0) cmd_spec
                       return (mck_val, Pid 0)
               else do (procH, w) ← makeProc inh' cmd_spec
                       p ← getPid' procH
                       penv p cmd_spec
                       (,p) ⊳ procWait cmd_spec (return (procH, w))

  let msg          = [fmtT|<CMD> «%w» %T|] p cmd_spec
      msgR         ∷ (ExitInfo, ξ) → [𝕋]
      msgR (ex, _) = [[fmtT|exit: %T|] (ex ⊣ exitVal)]
  logResult sev io_class mck msg (𝕵 msgR) (𝕽 result)

--------------------

{- | `systemx`, with `ω` specialized to `MockIOClass`. -}
systemx ∷ ∀ ε ζ ξ σ χ μ .
           (MonadIO μ, ToCmdSpec χ, HasCallStack,
            AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
            Printable ε, MonadError ε μ, HasCallStack,
            ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
            MonadLog (Log MockIOClass) μ) ⇒
           Severity        -- ^ Severity at which to log action
         → CmdRW           -- ^ Whether this is an 'active' or 'passive' command
         → (ExitStatus, ξ) -- ^ Dummy return values for when mocked
         → σ               -- ^ stdin specification
         → χ               -- ^ cmd + args
         → DoMock          -- ^ whether to mock this invocation
         → μ (ExitInfo, ξ)

systemx = systemx'

{-
λ> :m + Control.Monad.Log MonadIO.Process.ExitStatus FPath.AbsFile
        MockIO.OpenFile MonadIO.Process.CmdSpec MockIO MonadError Data.Function
        MonadIO.Error.CreateProcError Data.Either MockIO.Log
λ> let cspec = mkCmd [absfile|/run/current-system/sw/bin/grep|]
                     ["martyn","/etc/group"]
λ> logit' $ splitMError @ProcError @_ @(Either _)
          $ system Notice CmdR (ExitVal 0, ()) [absfile|/dev/null|] cspec NoMock
-}

{- | Like `system`, but collects all of the cmd specification (including mock /
     log bits) into MLCmdSpec.

     ε - error type
     α - type of input to be converted to an MLCmdSpec (e.g., (AbsFile,[𝕋]))
     ξ - type of output (e.g., () or [𝕋])
     ω - logging class
     ζ - type of output handles (inferred from ξ)
     σ - type of input (e.g., ℍ, or File, or [𝕋])
     μ - encompassing Monad
 -}
sys' ∷ ∀ ε α ξ ω ζ σ μ .
      (MonadIO μ, HasCallStack,
       ToMLCmdSpec α ξ, OutputDefault ξ, ToMaybeTexts ξ,
       OutputHandles ζ ξ, MakeProc ζ,
       AsIOError ε, AsFPathError ε, AsCreateProcError ε,
       AsProcExitError ε, Printable ε, MonadError ε μ,
       MLMakeIStream σ,
       Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
      σ → α → μ (ExitInfo, ξ)

sys' inh proto_mlc = do
  (mlc,msgs) ← toMLCmdSpec proto_mlc
  system' (mlc ⊣ severity) (mlc ⊣ cmdrw) (mlc ⊣ mock_value) inh
                           (mlc ⊣ cmdspec, msgs) (mlc ⊣ mock)

----------------------------------------

{- | Like `sys'`, but hardens the log class to `MockIOClass`, and pull the
     mock value from a surrounding Reader.

     N.B.: the mock value from the reader will override any set in the
           (inferred) MLCmdSpec.
-}
sys ∷ ∀ ε δ α ξ ζ σ μ .
      (MonadIO μ, HasCallStack, HasDoMock δ, MonadReader δ μ,
       ToMLCmdSpec α ξ, OutputDefault ξ, ToMaybeTexts ξ,
       OutputHandles ζ ξ, MakeProc ζ,
       AsIOError ε, AsFPathError ε, AsCreateProcError ε,
       AsProcExitError ε, Printable ε, MonadError ε μ,
       MLMakeIStream σ, MonadLog (Log MockIOClass) μ) ⇒
      σ → α → μ (ExitInfo, ξ)

sys inh proto_mlc = do
  mck        ← asks (view doMock)
  (mlc,msgs) ← toMLCmdSpec proto_mlc
  system' (mlc ⊣ severity) (mlc ⊣ cmdrw) (mlc ⊣ mock_value) inh
                           (mlc ⊣ cmdspec, msgs) mck

--------------------

{- | Alias for `sys` -}
(¡) ∷ ∀ ε δ α ξ ζ σ μ .
      (MonadIO μ, HasCallStack, HasDoMock δ, MonadReader δ μ,
       ToMLCmdSpec α ξ, OutputDefault ξ, ToMaybeTexts ξ,
       OutputHandles ζ ξ, MakeProc ζ,
       AsIOError ε, AsFPathError ε, AsCreateProcError ε,
       AsProcExitError ε, Printable ε, MonadError ε μ,
       MLMakeIStream σ, MonadLog (Log MockIOClass) μ) ⇒
      σ → α → μ (ExitInfo, ξ)

(¡) = sys

----------------------------------------

{-| Like `sys`, but implicitly takes `/dev/null` for input. -}

sysN ∷ ∀ ε δ α ξ ζ μ .
       (MonadIO μ, HasCallStack, HasDoMock δ, MonadReader δ μ,
        ToMLCmdSpec α ξ, OutputDefault ξ, ToMaybeTexts ξ,
        OutputHandles ζ ξ, MakeProc ζ,
        AsIOError ε, AsFPathError ε, AsCreateProcError ε,
        AsProcExitError ε, Printable ε, MonadError ε μ,
        MonadLog (Log MockIOClass) μ) ⇒
       α → μ (ExitInfo, ξ)
sysN c = devnull ≫ flip sys c

----------

{-| Alias for `sysN` -}
ꙩ ∷ ∀ ε δ α ξ ζ μ .
    (MonadIO μ, HasCallStack, HasDoMock δ, MonadReader δ μ,
     ToMLCmdSpec α ξ, OutputDefault ξ, ToMaybeTexts ξ,
     OutputHandles ζ ξ, MakeProc ζ,
     AsIOError ε, AsFPathError ε, AsCreateProcError ε,
     AsProcExitError ε, Printable ε, MonadError ε μ,
     MonadLog (Log MockIOClass) μ) ⇒
    α → μ (ExitInfo, ξ)
ꙩ = sysN

----------------------------------------

{-| Like `sys`, but implicitly takes `/dev/stdin` for input. -}
sysS ∷ ∀ ε δ α ξ ζ μ .
       (MonadIO μ, HasCallStack, HasDoMock δ, MonadReader δ μ,
        ToMLCmdSpec α ξ, OutputDefault ξ, ToMaybeTexts ξ,
        OutputHandles ζ ξ, MakeProc ζ,
        AsIOError ε, AsFPathError ε, AsCreateProcError ε,
        AsProcExitError ε, Printable ε, MonadError ε μ,
        MonadLog (Log MockIOClass) μ) ⇒
       α → μ (ExitInfo, ξ)
sysS = sys stdin

----------

{-| Alias for `sysS` -}
ꙫ ∷ ∀ ε δ α ξ ζ μ .
    (MonadIO μ, HasCallStack, HasDoMock δ, MonadReader δ μ,
     ToMLCmdSpec α ξ, OutputDefault ξ, ToMaybeTexts ξ,
     OutputHandles ζ ξ, MakeProc ζ,
     AsIOError ε, AsFPathError ε, AsCreateProcError ε,
     AsProcExitError ε, Printable ε, MonadError ε μ,
     MonadLog (Log MockIOClass) μ) ⇒
    α → μ (ExitInfo, ξ)
ꙫ = sysS

{-

λ> :m + FPath.AbsFile MonadIO.Error.CreateProcError Data.Either Data.Text
        Control.Monad.Log MockIO MonadError Data.Function MockIO.Log

λ> let mlc = mkMLCmd Notice CmdR [absfile|/run/current-system/sw/bin/grep|]
                                 ["martyn","/etc/group"] NoMock

λ> logit' $ splitMError @ProcError @_ @(Either _)
          $ sys @_ @_ @Text [absfile|/dev/null|] mlc

------------------------------------------------------------

λ> :m + FPath.AbsFile MonadIO.Error.CreateProcError Data.Either Data.Text
        Control.Monad.Log MockIO MonadError Data.Function MockIO.Log
        MockIO.Process.MLCmdSpec MonadIO.Process.CmdSpec ContainersPlus.Insert

λ> let mlc = mkMLCmdR [absfile|/run/current-system/sw/bin/grep|]
                      ["martyn","/etc/group"] NoMock & expExitVal <& 1

λ> Right (Right (ev,(t::Text))) <- logit' $ splitMError @ProcError @(Either _)
                                          $ [absfile|/dev/null|] ! mlc

λ> Data.Text.IO.putStrLn t

-}

{- | Alias for `sys` -}
{-
(!) ∷ ∀ ε ζ ξ σ μ .
      (MonadIO μ,
       AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
       Printable ε, MonadError ε μ, HasCallStack,
       ToMaybeTexts ξ, OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
       MonadLog (Log MockIOClass) μ) ⇒
      σ → MLCmdSpec ξ → μ (ExitInfo, ξ)
(!) = sys
-}

----------------------------------------

{- | Spawn a process; return the exit value, throw on signal.  The `finally`
     argument is always executed immediately after the process returns (whatever
     the exit value).
 -}
doProc' ∷ ∀ ε ζ ξ σ ω χ μ .
         (MonadIO μ, HasCallStack, MLMakeIStream σ, ToCmdSpec χ,
          ToMaybeTexts ξ, MakeProc ζ, OutputHandles ζ ξ,
          AsProcExitError ε, AsCreateProcError ε, AsFPathError ε, AsIOError ε,
          Printable ε, MonadError ε μ, HasDoMock ω, HasIOClass ω, Default ω,
          MonadLog (Log ω) μ) ⇒
         Severity → CmdRW → μ () → (ExitStatus, ξ) → σ → χ → DoMock
       → μ (ExitInfo, ξ)
doProc' sev rw finally mck_val input cspec mck = do
  result ← systemx' sev rw mck_val input cspec mck
  finally
  throwSig' (𝕽 result)

----------------------------------------

{- | `doProc'` with `ω` hardened to `MockIOClass`  -}
doProc ∷ ∀ ε ζ ξ σ χ μ .
        (MonadIO μ, HasCallStack, MLMakeIStream σ, ToCmdSpec χ,
         ToMaybeTexts ξ, MakeProc ζ, OutputHandles ζ ξ,
         AsProcExitError ε, AsCreateProcError ε, AsFPathError ε, AsIOError ε,
         Printable ε, MonadError ε μ, MonadLog (Log MockIOClass) μ) ⇒
        Severity → CmdRW → μ () → (ExitStatus, ξ) → σ → χ → DoMock
      → μ (ExitInfo, ξ)

doProc = doProc'

-- that's all, folks! ----------------------------------------------------------
