module MockIO.OpenFile
  ( FileOpenMode(..), HEncoding( Binary, NoEncoding, UTF8 )
  , appendFile, openFile, readFile, readFileY, readFileUTF8Lenient, withFile
  , writeFile, writeNoTruncFile, writeExFile

  , appendFlags, readFlags, readWriteExFlags, readWriteFlags
  , readWriteNoTruncFlags, writeExFlags, writeFlags, writeNoTruncFlags
  )
where

-- base --------------------------------

import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO )
import GHC.Stack               ( HasCallStack )
import Data.Function           ( ($) )
import Data.Maybe              ( fromMaybe )
import System.IO               ( IO
                               , IOMode( AppendMode, ReadMode, ReadWriteMode
                                       , WriteMode )
                               )
import System.Posix.Types      ( FileMode )

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- data-default ------------------------

import Data.Default  ( Default )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- fpath -------------------------------

import FPath.File  ( File, FileAs( _File_ ) )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Review  ( review )
import Control.Lens.Tuple   ( _1 )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog , Severity )

-- mockio ------------------------------

import MockIO  ( DoMock )

-- mockio-log --------------------------

import MockIO.Log      ( HasDoMock, mkIOLMER )
import MockIO.IOClass  ( HasIOClass, IOClass( IORead, IOWrite ) )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( AsIOError, squashNoSuchThingT )

-- monadio-plus ------------------------

import qualified MonadIO.File

import MonadIO.NamedHandle  ( ℍ, HWriteContents( hWriteContents )
                            , HGetContents( hGetContents )
                            , impliedEncoding, impliedEncodingM  )
import MonadIO.OpenFile     ( FileOpenMode(..)
                            , HEncoding( Binary, NoEncoding, UTF8 )
                            , appendFlags, fileOpenMode
                            , readFlags, readWriteExFlags, readWriteNoTruncFlags
                            , readWriteFlags, writeExFlags, writeFlags
                            , writeNoTruncFlags
                            )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕹 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )
import Control.Monad.Trans   ( lift )

-- text --------------------------------

import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

type 𝔹𝕊 = ByteString

------------------------------------------------------------

doFile ∷ ∀ ε α γ ω μ .
         (MonadIO μ, FileAs γ,
          Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
         Severity → 𝕄 (File → 𝕋) → ExceptT ε IO α → FileOpenMode → α → γ
       → DoMock → μ α
doFile sev msgf io mode mock_value (review _File_ → fn) mck =
  let (mt,ioc) = case (view _1) $ fileOpenMode mode of
                   ReadMode      → ("read ", IORead)
                   ReadWriteMode → ("rewrt", IOWrite)
                   WriteMode     → ("write", IOWrite)
                   AppendMode    → ("appnd", IOWrite)
      msg     = fromMaybe ([fmt|%t %T|] mt) msgf fn
   in mkIOLMER sev ioc msg 𝕹 mock_value io mck

----------------------------------------

openFile ∷ ∀ ε γ ω μ .
           (MonadIO μ, FileAs γ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω)⇒
           Severity → 𝕄 (File → 𝕋)
         → HEncoding → FileOpenMode → μ ℍ → γ → DoMock → μ ℍ
openFile sev msgf enc fomode a (review _File_ → fn) mck =
  let go = MonadIO.File.openFile enc fomode fn
   in join $ doFile sev msgf (ѥ go) fomode a fn mck

----------------------------------------

withFile ∷ ∀ ε α γ ω μ .
           (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω, FileAs γ)⇒
           Severity → 𝕄 (File → 𝕋) → HEncoding → FileOpenMode → μ α → γ
         → (ℍ → ExceptT ε IO α) → DoMock → μ α
withFile sev msgf enc fomode a fn io mck =
  let go = MonadIO.File.withFile enc fomode fn io
   in join $ doFile sev msgf (ѥ go) fomode a fn mck

----------------------------------------

readFile ∷ forall ε τ γ ω μ .
           (MonadIO μ, HGetContents τ, FileAs γ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
           Severity → 𝕄 (File → 𝕋) → μ τ → γ → DoMock → μ τ

readFile sev msgf a fn mck =
  let result = withFile sev msgf enc FileR a fn hGetContents mck
      enc    = impliedEncodingM result
   in result

--------------------

readFileY ∷ forall ε τ γ ω μ .
           (MonadIO μ, HGetContents τ, FileAs γ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
           Severity → 𝕄 (File → 𝕋) → τ → γ → DoMock → μ (𝕄 τ)

readFileY sev msgf a fn mck =
  squashNoSuchThingT $ readFile sev msgf (return a) fn mck

----------------------------------------

appendFile ∷ forall ε τ γ ω μ .
             (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, FileAs γ,
              MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
              HWriteContents τ) =>
             Severity → 𝕄 (File → 𝕋) → 𝕄 FileMode → γ → τ → DoMock → μ ()

appendFile sev msgf perms fn t =
  let write h = lift $ hWriteContents h t
   in withFile sev msgf (impliedEncoding t) (FileA perms) (return ()) fn write

-- λ> logit' $ appendFile @IOError @Text @_ @MockIOClass Notice Nothing Nothing [absfile|/tmp/bob|] ("bobob") NoMock

----------------------------------------

writeFile ∷ forall ε τ γ ω μ .
             (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, FileAs γ,
              MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
              HWriteContents τ) =>
             Severity → 𝕄 (File → 𝕋) → 𝕄 FileMode → γ → τ → DoMock → μ ()

writeFile sev msgf perms fn t =
  let write h = lift $ hWriteContents h t
   in withFile sev msgf (impliedEncoding t) (FileW perms) (return ()) fn write

--------------------

writeNoTruncFile ∷ forall ε τ γ ω μ .
             (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, FileAs γ,
              MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
              HWriteContents τ) =>
             Severity → 𝕄 (File → 𝕋) → 𝕄 FileMode → γ → τ → DoMock → μ ()

writeNoTruncFile sev msgf perms fn t =
  let write h = lift $ hWriteContents h t
   in withFile sev msgf
               (impliedEncoding t) (FileWNoTrunc perms) (return ()) fn write


--------------------

writeExFile ∷ forall ε τ γ ω μ .
             (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, FileAs γ,
              MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
              HWriteContents τ) =>
             Severity → 𝕄 (File → 𝕋) → FileMode → γ → τ → DoMock → μ ()

writeExFile sev msgf perms fn t =
  let write h = lift $ hWriteContents h t
   in withFile sev msgf (impliedEncoding t) (FileWEx perms) (return ()) fn write

----------------------------------------

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Replace any invalid input bytes with the Unicode replacement character
     U+FFFD.  Note that the mock value is given as a bytestring and decoded
     every time.
-}
readFileUTF8Lenient ∷ ∀ ε γ ω μ .
                      (MonadIO μ, FileAs γ,
                       MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω,
                       AsIOError ε, Printable ε, MonadError ε μ, HasCallStack)⇒
                      Severity → 𝕄 (File → 𝕋) → μ 𝔹𝕊 → γ → DoMock → μ 𝕋

readFileUTF8Lenient sev msgf a fn mck =
  decodeUtf8With lenientDecode ⊳ readFile sev msgf a fn mck

-- that's all, folks! ----------------------------------------------------------
