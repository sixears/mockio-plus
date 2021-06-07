{-# LANGUAGE UndecidableInstances #-} -- required for FileAs ⇒ MLMakeIStream

module MockIO.Process.MLMakeIStream
  {- | Like `MonadIO.Process.MakeInputStream`, but with mocking & logging. -}
  ( MLMakeIStream( makeIStream ) )
where

-- base --------------------------------

import Control.Monad.IO.Class  ( MonadIO )
import GHC.Stack               ( HasCallStack )
import System.IO               ( Handle )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( FileAs( _File_ ) )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- log-plus ----------------------------

import Log  ( Log, logIO )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock( DoMock, NoMock ), HasDoMock )

-- mockio-log --------------------------

import MockIO.IOClass  ( HasIOClass, IOClass( IOCmdR, IOCmdW ) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO.File  ( devnull )

-- more-unicode ------------------------

import Data.MoreUnicode.Maybe( pattern 𝕹 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- process -----------------------------

import System.Process  ( StdStream( UseHandle ) )

-- text-fmt ----------------------------

import Text.Fmt  ( fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.OpenFile  ( FileOpenMode( FileR ), HEncoding( NoEncoding )
                        , openFile )

--------------------------------------------------------------------------------

type ℍ = Handle

class MLMakeIStream α where
  makeIStream ∷ ∀ ε ω μ .
                (MonadIO μ, HasCallStack,
                 AsIOError ε, AsFPathError ε, MonadError ε μ, Printable ε,
                 HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                Severity → α → DoMock → μ ℍ

-- instance MLMakeIStream ℍ where

instance FileAs γ ⇒ MLMakeIStream γ where
  makeIStream sev (review _File_ → fn) mck =
    let plog t DoMock = "(" ⊕ t ⊕ ")"
        plog t NoMock = t
        msg fn = plog ([fmtT|opening %T for use as input stream|] fn)
     in do logIO sev def (msg fn mck)
           openFile sev 𝕹 NoEncoding FileR devnull fn mck

-- that's all, folks! ----------------------------------------------------------
