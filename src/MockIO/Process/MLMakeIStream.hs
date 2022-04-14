{-# LANGUAGE UndecidableInstances #-} -- required for FileAs ⇒ MLMakeIStream

{- | Like `MonadIO.Process.MakeInputStream`, but with mocking & logging. -}
module MockIO.Process.MLMakeIStream
  ( MLMakeIStream( makeIStream ) )
where

import Base1T  hiding  ( init, tail )

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- fpath -------------------------------

import FPath.AbsFile           ( absfile )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( File( FileA ), FileAs( _File_ ) )

-- log-plus ----------------------------

import Log  ( Log, logIO )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Debug ) )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock( DoMock, NoMock ), HasDoMock )

-- mockio-log --------------------------

import MockIO.IOClass  ( HasIOClass )

-- monadio-plus ------------------------

import MonadIO.File                   ( devnull )
import MonadIO.NamedHandle            ( ℍ )
import MonadIO.Process.MkInputStream  ( mkIStream )

-- process -----------------------------

import System.Process  ( StdStream )

-- safe --------------------------------

import Safe  ( succDef )

-- text --------------------------------

import Data.Text  ( init, length, pack, tail, take, unlines )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.OpenFile  ( FileOpenMode( FileR ), HEncoding( NoEncoding )
                        , openFile )

--------------------------------------------------------------------------------

type 𝔹𝕊 = ByteString

------------------------------------------------------------

plog ∷ 𝕋 → DoMock → 𝕋
plog t DoMock = "(" ⊕ t ⊕ ")"
plog t NoMock = t

------------------------------------------------------------

class MLMakeIStream α where
  makeIStream ∷ ∀ ε ω μ .
                (MonadIO μ, HasCallStack,
                 AsIOError ε, AsFPathError ε, MonadError ε μ, Printable ε,
                 HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                Severity → α → DoMock → μ StdStream

instance MLMakeIStream StdStream where
  makeIStream sev s mck = do
    logIO sev def (plog "using provided StdStream as input stream" mck)
    mkIStream s

instance MLMakeIStream 𝕋 where
  makeIStream sev t mck = do
    let pshow = tail ∘ init ∘ pack ∘ show
    let trim n (pshow → s) = if n ≤ length s
                             then s
                             else take (n-1) s ⊕ "…"
        msg = [fmtT|using provided Text «%t» as input stream|] (trim 40 t)
    logIO sev def (plog msg mck)
    mkIStream t

instance MLMakeIStream [𝕋] where
  makeIStream sev ts mck = makeIStream sev (unlines ts) mck

instance MLMakeIStream 𝔹𝕊 where
  makeIStream sev b mck = do
    logIO sev def (plog "using provided ByteStream as input stream" mck)
    mkIStream b

instance MLMakeIStream ℍ where
  makeIStream sev h mck = do
    logIO sev def (plog [fmtT|using pre-ordained ℍ as input stream|] mck)
    mkIStream h

instance {-# OVERLAPPABLE #-} FileAs γ ⇒ MLMakeIStream γ where
  makeIStream sev (review _File_ → fn) mck =
    let -- Add 1 to severity for using /dev/null as input: it's just not that
        -- interesting.
        sev' = if FileA [absfile|/dev/null|] ≡ fn
               then succDef Debug sev
               else sev
        msg n = plog ([fmtT|opening %T for use as input stream|] n)
     in do logIO sev' def (msg fn mck)
           openFile sev' 𝕹 NoEncoding FileR devnull fn mck ≫ mkIStream

-- that's all, folks! ----------------------------------------------------------
