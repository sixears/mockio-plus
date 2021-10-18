module MockIO.Flock
  ( SharedExclusive(..), flock, flockNB, unflock )
where

import Prelude ( undefined )

-- base --------------------------------

import Data.Function  ( const )
import Data.Maybe     ( maybe )

-- data-default ------------------------

import Data.Default  ( Default )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- filelock ----------------------------

import System.FileLock  ( SharedExclusive(..) )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath )
import FPath.File        ( FileAs )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog , Severity )

-- mockio-log --------------------------

import MockIO.IOClass      ( HasIOClass, IOClass( IOWrite ) )
import MockIO.Log          ( DoMock, HasDoMock, mkIOLMER )

-- monadio-plus ------------------------

import qualified  MonadIO.Flock  as  Flock

import MonadIO        ( MonadIO )
import MonadIO.Flock  ( NamedFileLock, flName )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣) )
import Data.MoreUnicode.Maybe  ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Text   ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

{- | Perform a (blocking) `flock` operation on a given file. -}
flock ∷ ∀ ε γ ω μ .
        (MonadIO μ, Printable ε, MonadError ε μ, MonadLog (Log ω) μ,
         Default ω, HasIOClass ω, HasDoMock ω,
         FileAs γ, Printable γ, AsFilePath γ) ⇒
        Severity → SharedExclusive → γ → DoMock → μ NamedFileLock
flock sev mode fn do_mock =
  let msg = [fmt|flock: '%T'|] fn
      lockf = Flock.flock mode fn
      return_msg ∷ NamedFileLock → [𝕋]
      return_msg _ = ["lock succeeded"]
   in mkIOLMER sev IOWrite msg (𝕵 return_msg) undefined lockf do_mock

----------------------------------------

{- | Attempt a `flock` without blocking on a given file. -}
flockNB ∷ ∀ ε γ ω μ .
          (MonadIO μ, Printable ε, MonadError ε μ, MonadLog (Log ω) μ,
           Default ω, HasIOClass ω, HasDoMock ω,
           FileAs γ, Printable γ, AsFilePath γ) ⇒
          Severity → SharedExclusive → γ → DoMock → μ (𝕄 NamedFileLock)
flockNB sev mode fn do_mock =
  let msg = [fmt|flknb: '%T'|] fn
      lockf = Flock.flockNB mode fn
      return_msg ∷ 𝕄 NamedFileLock → [𝕋]
      return_msg = maybe ["lock failed"] (const ["lock succeeded"])
   in mkIOLMER sev IOWrite msg (𝕵 return_msg) 𝕹 lockf do_mock

----------------------------------------

unflock ∷ ∀ ε ω μ .
          (MonadIO μ, Printable ε, MonadError ε μ,
           HasIOClass ω, Default ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
          Severity → NamedFileLock → DoMock → μ ()
unflock sev l do_mock =
  let msg = [fmt|unflock '%T'|] (l ⊣ flName)
   in mkIOLMER sev IOWrite msg 𝕹 () (Flock.unflock l) do_mock

-- that's all, folks! ----------------------------------------------------------
