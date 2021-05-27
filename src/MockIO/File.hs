{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MockIO.File
  ( AccessMode(..), FExists(..),
    access, chmod, fexists, fexists', lfexists, lfexists'
  , fileWritable, isWritableDir, isWritableFile
  , lstat, stat
  , unlink
  , writable

  , openFile, openFileBinary, openFileUTF8
  , openFileReadBinary, openFileReadWriteBinary, openFileReadWriteExBinary
  , openFileReadWriteNoTruncBinary
  , openFileWriteBinary, openFileWriteExBinary, openFileWriteNoTruncBinary
  , openFileAppendBinary

  , openFileReadUTF8, openFileReadWriteUTF8, openFileReadWriteExUTF8
  , openFileReadWriteNoTruncUTF8
  , openFileWriteUTF8, openFileWriteExUTF8, openFileWriteNoTruncUTF8
  , openFileAppendUTF8

  , withFile, withFileME, withFileUTF8, withFileBinary

  , withReadFileBinary, withReadWriteFileBinary, withReadWriteExFileBinary
  , withReadWriteNoTruncFileBinary
  , withWriteFileBinary, withWriteExFileBinary, withWriteNoTruncFileBinary
  , withAppendFileBinary

  , withReadFileUTF8, withReadWriteFileUTF8, withReadWriteExFileUTF8
  , withReadWriteNoTruncFileUTF8
  , withWriteFileUTF8, withWriteExFileUTF8, withWriteNoTruncFileUTF8
  , withAppendFileUTF8

  , readFileBinary, writeFileBinary, writeExFileBinary, writeNoTruncFileBinary
  , appendFileBinary

  , readFileUTF8, writeFileUTF8, writeNoTruncFileUTF8, writeExFileUTF8
  , appendFileUTF8

  , readFileUTF8Lenient
  , fileFoldLinesUTF8

  , appendFlags, readFlags, readWriteFlags, readWriteExFlags
  , readWriteNoTruncFlags, writeFlags, writeExFlags, writeNoTruncFlags
  )
where

-- base --------------------------------

import Control.Applicative     ( pure )
import Control.Monad           ( forM_, join, return )
import Data.Either             ( Either( Left, Right ) )
import Data.Function           ( ($), (&) )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe, maybe )
import Control.Monad.IO.Class  ( MonadIO )
import GHC.Stack               ( HasCallStack )
import System.IO               ( Handle, IO, IOMode( AppendMode, ReadMode
                                                   , ReadWriteMode, WriteMode )
                               , NewlineMode, TextEncoding
                               , char8, nativeNewlineMode, noNewlineTranslation
                               , utf8
                               )
import System.Posix.Types      ( FileMode )
import Text.Show               ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode  ( (⊕) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath )
import FPath.File        ( File, FileAs( _File_ ) )
import FPath.Dir         ( DirAs )

-- fstat -------------------------------

import FStat  ( FStat )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- log-plus ----------------------------

import Log  ( Log, logIO )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog
                          , Severity( Informational, Notice ) )

-- mockio ------------------------------

import MockIO  ( DoMock( DoMock, NoMock ) )

-- mockio-log --------------------------

import MockIO.Log      ( HasDoMock, doMock, mkIOL )
import MockIO.IOClass  ( HasIOClass, IOClass( IORead, IOWrite ), ioClass )

-- monadio-error -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import qualified  MonadIO.File
import MonadIO.File  ( AccessMode(..), FExists(..)
                     , appendFlags, fileFoldLinesH, readFlags
                     , readWriteFlags, readWriteExFlags, readWriteNoTruncFlags
                     , writeFlags, writeExFlags, writeNoTruncFlags
                     )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⩺) )
import Data.MoreUnicode.Lens     ( (⊢) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text                 ( lines, pack )
import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- unix --------------------------------

import System.Posix.IO     ( OpenFileFlags )

--------------------------------------------------------------------------------

{- | Log a mockable IO Action, including its result (if provided a suitable
     formatter), and any exception it throws. -}
mkIOLMER ∷ (MonadIO μ, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
            Severity → IOClass → 𝕋 → 𝕄 (α → [𝕋]) → α
         → ExceptT ε IO α → DoMock → μ α
mkIOLMER sev ioclass msg valmsg mock_value io mck = do
  let stg  = def & ioClass ⊢ ioclass & doMock ⊢ mck
      pp ∷ DoMock → 𝕋 → 𝕋
      pp NoMock t = t
      pp DoMock t = "(" ⊕ t ⊕ ")"
  result ← mkIOL sev ioclass msg (Right mock_value) (ѥ io) mck
  case result of
    Left  e → do logIO sev stg (pp mck $ [fmtT|%t FAILED: %T|] msg e)
                 throwError e
    Right r → do case valmsg of
                   Nothing → return ()
                   Just v  → forM_ (v r) $ \ t →
                     logIO sev stg (pp mck $ [fmtT|%t: %t|] msg t)
                 return r

----------------------------------------

doFile ∷ (MonadIO μ, MonadLog (Log ω) μ, Printable ε, MonadError ε μ,
          HasCallStack, Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
         ExceptT ε IO α → IOMode → Severity → 𝕄 (File → 𝕋) → α → γ → DoMock
       → μ α
doFile io mode sev msgf mock_value (review _File_ → fn) mck =
  let (mt,ioc) = case mode of
                   ReadMode      → ("read ", IORead)
                   ReadWriteMode → ("rewrt", IOWrite)
                   WriteMode     → ("write", IOWrite)
                   AppendMode    → ("appnd", IOWrite)
      msg     = fromMaybe ([fmt|%t %T|] mt) msgf fn
   in mkIOLMER sev ioc msg Nothing mock_value io mck

openFile ∷ (MonadIO μ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω, FileAs γ)⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → Severity
         → 𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock → μ Handle
openFile enc nlm mode flags sev msgf perms a (review _File_ → fn) mck =
  let go = MonadIO.File.openFile enc nlm mode flags perms fn
   in join $ doFile (ѥ go) mode sev msgf a fn mck

--------------------

openFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                  HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                  HasDoMock ω, FileAs γ) ⇒
                 IOMode → OpenFileFlags → Severity → 𝕄 (File → 𝕋)
               → 𝕄 FileMode → μ Handle → γ → DoMock → μ Handle
openFileBinary = openFile char8 noNewlineTranslation

--------------------

openFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                HasDoMock ω, FileAs γ) ⇒
               IOMode → OpenFileFlags → Severity → 𝕄 (File → 𝕋) → 𝕄 FileMode
             → μ Handle → γ → DoMock → μ Handle
openFileUTF8 = openFile utf8 nativeNewlineMode

----------------------------------------

openFileReadBinary ∷ (MonadIO μ,
                      AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                      Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ,
                      FileAs γ) ⇒
                     μ Handle → γ → DoMock → μ Handle
openFileReadBinary =
  openFileBinary ReadMode readFlags Informational Nothing Nothing

openFileReadWriteBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                           HasCallStack, MonadLog (Log ω) μ, Default ω,
                           HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                          𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                        → μ Handle
openFileReadWriteBinary = openFileBinary ReadWriteMode readWriteFlags Notice


openFileReadWriteExBinary ∷ (MonadIO μ, AsIOError ε, Printable ε,
                             MonadError ε μ, HasCallStack, MonadLog (Log ω) μ,
                             Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                            𝕄 (File → 𝕋) → FileMode → μ Handle → γ → DoMock
                          → μ Handle
openFileReadWriteExBinary msgf perms =
  openFileBinary ReadWriteMode readWriteExFlags Notice msgf (Just perms)

openFileReadWriteNoTruncBinary ∷ (MonadIO μ, AsIOError ε, Printable ε,
                                  MonadError ε μ, HasCallStack,
                                  MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                                  HasDoMock ω, FileAs γ) ⇒
                                 𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ
                               → DoMock → μ Handle
openFileReadWriteNoTruncBinary =
  openFileBinary ReadWriteMode readWriteNoTruncFlags Notice

openFileWriteBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                       HasCallStack, MonadLog (Log ω) μ, Default ω,
                       HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                      𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                    → μ Handle
openFileWriteBinary = openFileBinary WriteMode writeFlags Notice

openFileWriteExBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                         HasCallStack, MonadLog (Log ω) μ, Default ω,
                         HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                        𝕄 (File → 𝕋) → FileMode → μ Handle → γ → DoMock
                      → μ Handle
openFileWriteExBinary msgf perms =
  openFileBinary WriteMode writeExFlags Notice msgf (Just perms)

openFileWriteNoTruncBinary ∷ (MonadIO μ,
                              AsIOError ε, Printable ε, MonadError ε μ,
                              HasCallStack, MonadLog (Log ω) μ, Default ω,
                              HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                             𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                           → μ Handle
openFileWriteNoTruncBinary = openFileBinary WriteMode writeNoTruncFlags Notice

openFileAppendBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                        HasCallStack, MonadLog (Log ω) μ, Default ω,
                        HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                      𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                    → μ Handle
openFileAppendBinary = openFileBinary AppendMode appendFlags Notice

----------------------------------------

openFileReadUTF8 ∷ (MonadIO μ,
                    AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                    Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ,
                    FileAs γ) ⇒
                   μ Handle → γ → DoMock → μ Handle
openFileReadUTF8 = openFileUTF8 ReadMode readFlags Informational Nothing Nothing

openFileReadWriteUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                         HasCallStack, MonadLog (Log ω) μ, Default ω,
                         HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                        𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                      → μ Handle
openFileReadWriteUTF8 = openFileUTF8 ReadWriteMode readWriteFlags Notice

openFileReadWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                           HasCallStack, MonadLog (Log ω) μ, Default ω,
                           HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                          𝕄 (File → 𝕋) → FileMode → μ Handle → γ → DoMock
                        → μ Handle
openFileReadWriteExUTF8 msgf perms =
  openFileUTF8 ReadWriteMode readWriteExFlags Notice msgf (Just perms)

openFileReadWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε,
                                MonadError ε μ, HasCallStack,
                                MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                                HasDoMock ω, FileAs γ) ⇒
                               𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ
                             → DoMock → μ Handle
openFileReadWriteNoTruncUTF8 =
  openFileUTF8 ReadWriteMode readWriteNoTruncFlags Notice

openFileWriteUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                     HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                     HasDoMock ω, FileAs γ) ⇒
                    𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                  → μ Handle
openFileWriteUTF8 = openFileUTF8 WriteMode writeFlags Notice

openFileWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                       HasCallStack, MonadLog (Log ω) μ, Default ω,
                       HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                      𝕄 (File → 𝕋) → FileMode → μ Handle → γ → DoMock
                    → μ Handle
openFileWriteExUTF8 msgf perms =
  openFileUTF8 WriteMode writeExFlags Notice msgf (Just perms)

openFileWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                            HasCallStack, MonadLog (Log ω) μ, Default ω,
                            HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                           𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                         → μ Handle
openFileWriteNoTruncUTF8 = openFileUTF8 WriteMode writeNoTruncFlags Notice

openFileAppendUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                      HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                      HasDoMock ω, FileAs γ) ⇒
                     𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                   → μ Handle
openFileAppendUTF8 = openFileUTF8 AppendMode appendFlags Notice

----------------------------------------

withFile ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω, FileAs γ)⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → Severity
         → 𝕄 (File → 𝕋) → 𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withFile enc nlm mode flags sev msgf perms a (review _File_ → fn) io mck =
  let go = MonadIO.File.withFile enc nlm mode flags perms fn io
   in join $ doFile (ѥ go) mode sev msgf a fn mck

----------------------------------------

withFileME ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
              MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
              FileAs γ) ⇒
             TextEncoding → NewlineMode → IOMode → OpenFileFlags → Severity
           → 𝕄 (File → 𝕋) → 𝕄 FileMode → μ α → γ → (Handle → ExceptT ε IO α)
           → DoMock → μ α
withFileME enc nlm mode flags sev msgf perms a (review _File_ → fn) io mck =
  let go = MonadIO.File.withFileME enc nlm mode flags perms fn io
   in join $ doFile (ѥ go) mode sev msgf a fn mck

----------------------------------------

withFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                  HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                  HasDoMock ω, FileAs γ) ⇒
                 IOMode → OpenFileFlags → Severity → 𝕄 (File → 𝕋) → 𝕄 FileMode
               → μ α → γ → (Handle → IO α) → DoMock → μ α
withFileBinary = withFile char8 noNewlineTranslation

withFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                HasDoMock ω, FileAs γ) ⇒
               IOMode → OpenFileFlags → Severity → 𝕄 (File → 𝕋) → 𝕄 FileMode
             → μ α → γ → (Handle → IO α) → DoMock → μ α
withFileUTF8 = withFile utf8 nativeNewlineMode

----------------------------------------

withReadFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                      HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                      HasDoMock ω, FileAs γ) ⇒
                     μ α → γ → (Handle → IO α) → DoMock → μ α
withReadFileBinary =
  withFileBinary ReadMode readFlags Informational Nothing Nothing

----------

withReadWriteFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                           HasCallStack, MonadLog (Log ω) μ, Default ω,
                           HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                          𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withReadWriteFileBinary =
  withFileBinary ReadWriteMode readWriteFlags Notice Nothing

----------

withReadWriteExFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε,
                             MonadError ε μ, HasCallStack, MonadLog (Log ω) μ,
                             Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                            FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withReadWriteExFileBinary perms =
  withFileBinary ReadWriteMode readWriteExFlags Notice Nothing (Just perms)

----------

withReadWriteNoTruncFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε,
                                  MonadError ε μ, HasCallStack,
                                  MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                                  HasDoMock ω, FileAs γ) ⇒
                                 𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock
                               → μ α
withReadWriteNoTruncFileBinary =
  withFileBinary ReadWriteMode readWriteNoTruncFlags Notice Nothing

----------

withWriteFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                       HasCallStack, MonadLog (Log ω) μ, Default ω,
                       HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                      𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteFileBinary =
  withFileBinary WriteMode writeFlags Notice Nothing

----------

withWriteExFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                         HasCallStack, MonadLog (Log ω) μ, Default ω,
                         HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                        FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteExFileBinary perms =
  withFileBinary WriteMode writeExFlags Notice Nothing (Just perms)

----------

withWriteNoTruncFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε,
                              MonadError ε μ, HasCallStack, MonadLog (Log ω) μ,
                              Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                             𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock
                           → μ α
withWriteNoTruncFileBinary =
  withFileBinary WriteMode writeNoTruncFlags Notice Nothing

----------

withAppendFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                        HasCallStack, MonadLog (Log ω) μ, Default ω,
                        HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                      𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withAppendFileBinary =
  withFileBinary AppendMode appendFlags Notice Nothing

----------------------------------------

withReadFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                    HasCallStack, Default ω, HasIOClass ω, HasDoMock ω,
                    MonadLog (Log ω) μ, FileAs γ) ⇒
                   μ α → γ → (Handle → IO α) → DoMock → μ α
withReadFileUTF8 =
  withFileUTF8 ReadMode readFlags Informational Nothing Nothing

----------

withReadWriteFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                         HasCallStack, MonadLog (Log ω) μ, Default ω,
                         HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                        𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withReadWriteFileUTF8 =
  withFileUTF8 ReadWriteMode readWriteFlags Notice Nothing

----------

withReadWriteExFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                           HasCallStack, MonadLog (Log ω) μ, Default ω,
                           HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                          FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withReadWriteExFileUTF8 perms =
  withFileUTF8 ReadWriteMode readWriteExFlags Notice Nothing (Just perms)

----------

withReadWriteNoTruncFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε,
                                MonadError ε μ, HasCallStack,
                                MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                                HasDoMock ω, FileAs γ) ⇒
                               𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock
                             → μ α
withReadWriteNoTruncFileUTF8 =
  withFileUTF8 ReadWriteMode readWriteNoTruncFlags Notice Nothing

----------

withWriteFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                     HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                     HasDoMock ω, FileAs γ) ⇒
                    𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteFileUTF8 =
  withFileUTF8 WriteMode writeFlags Notice Nothing

----------

withWriteExFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                       HasCallStack, MonadLog (Log ω) μ, Default ω,
                       HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                      FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteExFileUTF8 perms =
  withFileUTF8 WriteMode writeExFlags Notice Nothing (Just perms)

----------

withWriteNoTruncFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                            HasCallStack, MonadLog (Log ω) μ, Default ω,
                            HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                           𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteNoTruncFileUTF8 =
  withFileUTF8 WriteMode writeNoTruncFlags Notice Nothing

----------

withAppendFileUTF8 ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                      HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                      HasDoMock ω, FileAs γ) ⇒
                     𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withAppendFileUTF8 =
  withFileUTF8 AppendMode appendFlags Notice Nothing

----------------------------------------

{- | Read a file as bytes. -}
readFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                  HasCallStack, FileAs γ, MonadLog (Log ω) μ, Default ω,
                  HasDoMock ω, HasIOClass ω) ⇒
                 μ ByteString → γ → DoMock → μ ByteString
readFileBinary a fn = withReadFileBinary a fn BS.hGetContents

----------

writeFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                   HasCallStack, FileAs γ, MonadLog (Log ω) μ, Default ω,
                   HasDoMock ω, HasIOClass ω) ⇒
                  𝕄 FileMode → γ → ByteString → DoMock → μ ()
writeFileBinary perms fn txt =
  withWriteFileBinary perms (return ()) fn (\ h → BS.hPutStr h txt)

----------

writeNoTruncFileBinary ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                          HasCallStack, FileAs γ, MonadLog (Log ω) μ, Default ω,
                          HasDoMock ω, HasIOClass ω) ⇒
                         𝕄 FileMode → γ → ByteString → DoMock → μ ()
writeNoTruncFileBinary perms fn txt =
  withWriteNoTruncFileBinary perms (return ()) fn (\ h → BS.hPutStr h txt)

----------

writeExFileBinary ∷ (MonadIO μ,
                     AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                     FileAs γ,
                     MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                    FileMode → γ → ByteString → DoMock → μ ()
writeExFileBinary perms fn txt =
  withWriteExFileBinary perms (return ()) fn (\ h → BS.hPutStr h txt)

----------

appendFileBinary ∷ (MonadIO μ,
                    AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                    FileAs γ,
                   MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                  𝕄 FileMode → γ → ByteString → DoMock → μ ()
appendFileBinary perms fn txt =
  withAppendFileBinary perms (return ()) fn (\ h → BS.hPutStr h txt)

----------------------------------------

readFileUTF8 ∷ (MonadIO μ,
                AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                FileAs γ,
                MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
               μ 𝕋 → γ → DoMock → μ 𝕋
readFileUTF8 a fn = withReadFileUTF8 a fn TextIO.hGetContents

----------

readFileUTF8Lenient ∷ (MonadIO μ,
                       AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                       MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω,
                       FileAs γ) ⇒
                      μ 𝕋 → γ → DoMock → μ 𝕋
readFileUTF8Lenient a fn =
  withReadFileUTF8 a fn (decodeUtf8With lenientDecode ⩺ BS.hGetContents)

----------

writeFileUTF8 ∷ (MonadIO μ,
                 AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                 FileAs γ,
                 MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                𝕄 FileMode → γ → 𝕋 → DoMock → μ ()
writeFileUTF8 perms fn txt =
  withWriteFileUTF8 perms (return ()) fn (\ h → TextIO.hPutStr h txt)

----------

writeNoTruncFileUTF8 ∷ (MonadIO μ,
                        AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                        FileAs γ,
                        MonadLog (Log ω) μ, Default ω, HasDoMock ω,
                        HasIOClass ω) ⇒
                       𝕄 FileMode → γ → 𝕋 → DoMock → μ ()
writeNoTruncFileUTF8 perms fn txt =
  withWriteNoTruncFileUTF8 perms (return ()) fn (\ h → TextIO.hPutStr h txt)

----------

writeExFileUTF8 ∷ (MonadIO μ,
                   AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                   FileAs γ,
                   MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                  FileMode → γ → 𝕋 → DoMock → μ ()
writeExFileUTF8 perms fn txt =
  withWriteExFileUTF8 perms (return ()) fn (\ h → TextIO.hPutStr h txt)

----------

appendFileUTF8 ∷ (MonadIO μ,
                  AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                  FileAs γ,
                  MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                 𝕄 FileMode → γ → 𝕋 → DoMock → μ ()
appendFileUTF8 perms fn txt =
  withAppendFileUTF8 perms (return ()) fn (\ h → TextIO.hPutStr h txt)

----------------------------------------

{- | Work over a file, accumulating results, line-by-line. -}
fileFoldLinesUTF8 ∷ (MonadIO μ, FileAs γ,
                     AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                     MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                     α → (α → 𝕋 → IO α) → μ α → γ → DoMock → μ α
fileFoldLinesUTF8 a io w fn = withReadFileUTF8 w fn $ fileFoldLinesH a io

----------------------------------------

fexists ∷ (MonadIO μ,
           AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
           MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
           AsFilePath ρ, Printable ρ) ⇒
          Severity → FExists → ρ → DoMock → μ FExists
fexists sev mock_value fn = do
  let msg = [fmt|fxist %T|] fn
      vmsg = Just $ (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.fexists fn)

--------------------

fexists' ∷ (MonadIO μ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
            AsFilePath ρ, Printable ρ) ⇒
           Severity → FExists → ρ → DoMock → μ FExists
fexists' sev mock_value fn = do
  let msg = [fmt|fxst' %T|] fn
      vmsg = Just $ (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.fexists' fn)

--------------------

lfexists ∷ (MonadIO μ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
            AsFilePath ρ, Printable ρ) ⇒
           Severity → FExists → ρ → DoMock → μ FExists
lfexists sev mock_value fn = do
  let msg = [fmt|lfxst %T|] fn
      vmsg = Just $ (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.lfexists fn)

--------------------

lfexists' ∷ (MonadIO μ,
             AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
             MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
             AsFilePath ρ, Printable ρ) ⇒
            Severity → FExists → ρ → DoMock → μ FExists
lfexists' sev mock_value fn = do
  let msg = [fmt|lfxt' %T|] fn
      vmsg = Just $ (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.lfexists' fn)

----------------------------------------

access ∷ (MonadIO μ,
          AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
          AsFilePath ρ, Printable ρ) ⇒
         Severity → AccessMode → 𝕄 𝔹 → ρ → DoMock → μ (𝕄 𝔹)
access sev amode mock_value fn = do
  let msg = [fmt|access %T %w|] fn amode
      vmsg = Just $ maybe ["Nothing"] (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.access amode fn)

----------------------------------------

_stat ∷ (MonadIO μ, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         AsFilePath ρ, Printable ρ) ⇒
        (ρ → ExceptT ε IO (𝕄 FStat))
      → Severity → 𝕄 FStat → ρ → DoMock → μ (𝕄 FStat)
_stat s sev mock_value fn mck =
  let msg  = [fmt|stat  %T|] fn
      vmsg = Just $ maybe ["Nothing"] (lines ∘ toText)
   in mkIOLMER sev IORead msg vmsg mock_value (s fn) mck

--------------------

stat ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
        MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
        AsFilePath ρ, Printable ρ) ⇒
       Severity → 𝕄 FStat → ρ → DoMock → μ (𝕄 FStat)
stat = _stat MonadIO.File.stat

----------

lstat ∷ (MonadIO μ,
         AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         AsFilePath ρ, Printable ρ) ⇒
        Severity → 𝕄 FStat → ρ → DoMock → μ (𝕄 FStat)
lstat = _stat MonadIO.File.lstat

----------------------------------------

{- | Simple shortcut for file (or directory) is writable by this user; `Nothing`
     is returned if file does not exist. -}
writable ∷ (MonadIO μ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
            AsFilePath ρ, Printable ρ) ⇒
           Severity → 𝕄 𝔹 → ρ → DoMock → μ (𝕄 𝔹)
writable sev = access sev ACCESS_W

----------------------------------------

chmod ∷ (MonadIO μ,
         AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         AsFilePath ρ, Printable ρ) ⇒
        Severity → FileMode → ρ → DoMock → μ ()
chmod sev perms fn =
  let msg = [fmt|chmod %T %04o|] fn perms
   in mkIOLMER sev IOWrite msg Nothing () (MonadIO.File.chmod perms fn)

----------------------------------------

unlink ∷ (MonadIO μ,
          AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
          FileAs γ, Printable γ) ⇒
         Severity → γ → DoMock → μ ()
unlink sev fn =
  mkIOLMER sev IOWrite ([fmt|unlnk %T|] fn) Nothing () (MonadIO.File.unlink fn)

----------------------------------------

{- | Is `f` an extant writable file? -}
isWritableFile ∷ (MonadIO μ,
                  AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                  MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                  FileAs γ, Printable γ) ⇒
                 Severity → 𝕄 𝕋 → γ → DoMock → μ (𝕄 𝕋)

isWritableFile sev mock_value fn =
  let msg = [fmt|isWrF %T|] fn
      vmsg = Just $ maybe ["file is writable"] pure
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.isWritableFile fn)

----------------------------------------

{- | Is `f` an extant writable directory? -}
isWritableDir ∷ (MonadIO μ,
                 AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                 MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                 DirAs γ, Printable γ) ⇒
                Severity → 𝕄 𝕋 → γ → DoMock → μ (𝕄 𝕋)

isWritableDir sev mock_value fn =
  let msg = [fmt|isWrD %T|] fn
      vmsg = Just $ maybe ["file is writable"] pure
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.isWritableDir fn)

----------------------------------------

{- | Test that the given path is a writable (by this user) *file*, or does not
     exist but is in a directory that is writable & executable by this user.
     In case of not writable, some error text is returned to say why.
 -}
fileWritable ∷ (MonadIO μ,
                AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                FileAs γ, Printable γ) ⇒
               Severity → 𝕄 𝕋 → γ → DoMock → μ (𝕄 𝕋)
fileWritable sev mock_value fn =
  let msg = [fmt|filWr %T|] fn
      vmsg = Just $ maybe ["file is (potentially) writable"] pure
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.fileWritable fn)

-- that's all, folks! ----------------------------------------------------------
