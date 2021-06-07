module MockIO.Process
  ( CmdRW(..) )
where

-- base --------------------------------

import Control.Monad.IO.Class  ( MonadIO )
import System.IO               ( Handle )

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

import MockIO.IOClass  ( HasIOClass, IOClass( IOCmdR, IOCmdW ) )
import MockIO.Log      ( mkIOLMER )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import qualified  MonadIO.Process

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Process.CmdSpec        ( CmdSpec )
import MonadIO.Process.ExitStatus     ( ExitStatus )
import MonadIO.Process.MakeProc       ( MakeProc )
import MonadIO.Process.MkInputStream  ( MkInputStream( mkIStream ) )
import MonadIO.Process.OutputHandles  ( OutputHandles )

-- more-unicode ------------------------

import Data.MoreUnicode.Maybe  ( pattern 𝕹 )
import Data.MoreUnicode.Text   ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- process -----------------------------

import System.Process  ( StdStream )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.Process.MLMakeIStream ( MLMakeIStream( makeIStream ) )

--------------------------------------------------------------------------------

type ℍ = Handle

------------------------------------------------------------

{- | Marker for whether a cmd merely observes the environment - e.g., lists a
     file - or updates the environment, e.g., sets an env.var. -}
data CmdRW = CmdR | CmdW

ioc ∷ CmdRW → IOClass
ioc CmdR = IOCmdR
ioc CmdW = IOCmdW

------------------------------------------------------------

{- | Execute an external process, wait for termination, return exit status and
     whichever of stderr/stdout were implicitly requested by the return type. -}
system ∷ ∀ ε ζ ξ σ ω μ.
         (MonadIO μ,
          AsIOError ε, AsFPathError ε, AsCreateProcError ε,
          Printable ε, MonadError ε μ,
          OutputHandles ζ ξ, MakeProc ζ, MLMakeIStream σ,
          HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
         Severity        -- ^ Severity at which to log action
       → CmdRW           -- ^ Whether this is an 'active' or 'passive' command
       → (ExitStatus, ξ) -- ^ Dummy return values for when mocked
       → σ               -- ^ stdin specification
       → CmdSpec         -- ^ cmd + args
       → DoMock          -- ^ whether to mock this invocation
       → μ (ExitStatus, ξ)

system sev rw mck_val inh cspec mck = do
  inh' ← makeIStream sev inh mck
  let msg = [fmtT|<cmd> %T|] cspec
  mkIOLMER sev (ioc rw) msg 𝕹 {- (valmsg∷𝕄(α→[𝕋])) -} mck_val (MonadIO.Process.system inh' cspec) mck

{-
λ> :m + Control.Monad.Log MonadIO.Process.ExitStatus FPath.AbsFile MockIO.OpenFile MonadIO.Process.CmdSpec MockIO MonadError Data.Function MonadIO.Error.CreateProcError Data.Either MockIO.Log
λ> let cspec = mkCmd [absfile|/run/current-system/sw/bin/grep|] ["martyn","/etc/group"]
λ> logit' $ splitMError @ProcError @(Either _) $ system Notice CmdR (ExitVal 0, ()) [absfile|/dev/null|] cspec NoMock
-}

-- that's all, folks! ----------------------------------------------------------
