{- | Default values for processes (which typically return some combination
     of exit values and Texts/ByteStreams). -}

module MockIO.Process.OutputDefault
  ( OutputDefault( outDef ) )
where

-- bytestring --------------------------

import qualified  Data.ByteString

-- monadio-plus ------------------------

import MonadIO.Process.ExitStatus( ExitStatus( ExitVal ) )

-- more-unicode --------------------------------

import Data.MoreUnicode.Text  ( 𝕋 )

--------------------------------------------------------------------------------

type 𝔹𝕊 = Data.ByteString.ByteString

------------------------------------------------------------

{- | Default values for mocked process output; exit 0 for command, empty texts
     where used. -}
class OutputDefault ξ where
  outDef ∷ ξ

instance OutputDefault ExitStatus where
  outDef = ExitVal 0

instance OutputDefault () where
  outDef = ()

instance OutputDefault 𝕋 where
  outDef = ""

instance OutputDefault [𝕋] where
  outDef = []

instance OutputDefault 𝔹𝕊 where
  outDef = ""

instance (OutputDefault ξ, OutputDefault ξ') ⇒ OutputDefault (ξ, ξ') where
  outDef = (outDef, outDef)

-- that's all, folks! ----------------------------------------------------------
