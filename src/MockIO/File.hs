{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MockIO.File
  ( openFile, openFileBinary, openFileUTF8
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
  )
where

import Prelude ( undefined )

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( join, return )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe, maybe )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import System.IO               ( Handle, IO, IOMode( AppendMode, ReadMode
                                                   , ReadWriteMode, WriteMode )
                               , NewlineMode, TextEncoding
                               , char8, nativeNewlineMode, noNewlineTranslation
                               , stdout, utf8
                               )
import System.Posix.Types      ( FileMode )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- data-default ------------------------

import Data.Default  ( Default )

-- data-textual ------------------------

import Data.Textual  ( toString, toText )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile, absfile )
import FPath.File     ( File( FileA ), FileAs( _File_ ) )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational, Notice ) )

import MockIO  ( DoMock( NoMock ) )

-- mockio-log --------------------------

import MockIO.Log      ( HasDoMock, MockIOClass, mkIOL )
import MockIO.IOClass  ( HasIOClass, IOClass( IORead, IOWrite ) )

-- monadio-error -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( asIOError, asIOErrorY )
import MonadError.IO.Error  ( AsIOError, IOError, squashInappropriateTypeT )

-- monadio-plus ------------------------

import qualified  MonadIO.File
import MonadIO.File  ( appendFlags, devnull, fileFoldLinesH, readFlags
                     , readWriteFlags, readWriteExFlags, readWriteNoTruncFlags
                     , writeFlags, writeExFlags, writeNoTruncFlags
                     )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text                 ( Text, unpack )
import Data.Text.IO              ( hGetContents, hPutStr )
import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- unix --------------------------------

import System.Posix.IO  ( OpenFileFlags )

--------------------------------------------------------------------------------

doFile ∷ (MonadIO μ, MonadLog (Log ω) μ,
          Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
         IO α → IOMode → Severity → 𝕄 (File → 𝕋) → α → γ → DoMock → μ α
doFile go mode sev msgf a (review _File_ → fn) mck =
  let (mt,md) = case mode of
                  ReadMode      → ("read ", IORead)
                  ReadWriteMode → ("rewrt", IOWrite)
                  WriteMode     → ("write", IOWrite)
                  AppendMode    → ("appnd", IOWrite)
      msg     = fromMaybe ([fmt|%t %T|] mt) msgf fn
   in mkIOL sev md msg a go mck

openFile ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
            Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → Severity
         → 𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock → μ Handle
openFile enc nlm mode flags sev msgf perms a (review _File_ → fn) mck =
  let go = MonadIO.File.openFile enc nlm mode flags perms fn
   in join $ doFile (ѥ go) mode sev msgf a fn mck

--------------------

openFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
                  Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                 IOMode → OpenFileFlags → Severity → 𝕄 (File → 𝕋)
               → 𝕄 FileMode → μ Handle → γ → DoMock → μ Handle
openFileBinary = openFile char8 noNewlineTranslation

--------------------

openFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
                Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
               IOMode → OpenFileFlags → Severity → 𝕄 (File → 𝕋) → 𝕄 FileMode
             → μ Handle → γ → DoMock → μ Handle
openFileUTF8 = openFile utf8 nativeNewlineMode

----------------------------------------

openFileReadBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
                      Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                     μ Handle → γ → DoMock → μ Handle
openFileReadBinary =
  openFileBinary ReadMode readFlags Informational Nothing Nothing

openFileReadWriteBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                           MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                           HasDoMock ω, FileAs γ) ⇒
                          𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                        → μ Handle
openFileReadWriteBinary = openFileBinary ReadWriteMode readWriteFlags Notice


openFileReadWriteExBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                             MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                             HasDoMock ω, FileAs γ) ⇒
                            𝕄 (File → 𝕋) → FileMode → μ Handle → γ → DoMock
                          → μ Handle
openFileReadWriteExBinary msgf perms =
  openFileBinary ReadWriteMode readWriteExFlags Notice msgf (Just perms)

openFileReadWriteNoTruncBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                  MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                                  HasDoMock ω, FileAs γ) ⇒
                                 𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ
                               → DoMock → μ Handle
openFileReadWriteNoTruncBinary =
  openFileBinary ReadWriteMode readWriteNoTruncFlags Notice

openFileWriteBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                       MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                       HasDoMock ω, FileAs γ) ⇒
                      𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                    → μ Handle
openFileWriteBinary = openFileBinary WriteMode writeFlags Notice

openFileWriteExBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                         MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                         HasDoMock ω, FileAs γ) ⇒
                        𝕄 (File → 𝕋) → FileMode → μ Handle → γ → DoMock
                      → μ Handle
openFileWriteExBinary msgf perms =
  openFileBinary WriteMode writeExFlags Notice msgf (Just perms)

openFileWriteNoTruncBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                              MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                              HasDoMock ω, FileAs γ) ⇒
                             𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                           → μ Handle
openFileWriteNoTruncBinary = openFileBinary WriteMode writeNoTruncFlags Notice

openFileAppendBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                       MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                       HasDoMock ω, FileAs γ) ⇒
                      𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                    → μ Handle
openFileAppendBinary = openFileBinary AppendMode appendFlags Notice

----------------------------------------

openFileReadUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
                    Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                   μ Handle → γ → DoMock → μ Handle
openFileReadUTF8 = openFileUTF8 ReadMode readFlags Informational Nothing Nothing

openFileReadWriteUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                         MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                         HasDoMock ω, FileAs γ) ⇒
                        𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                      → μ Handle
openFileReadWriteUTF8 = openFileUTF8 ReadWriteMode readWriteFlags Notice

openFileReadWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                           MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                           HasDoMock ω, FileAs γ) ⇒
                          𝕄 (File → 𝕋) → FileMode → μ Handle → γ → DoMock
                        → μ Handle
openFileReadWriteExUTF8 msgf perms =
  openFileUTF8 ReadWriteMode readWriteExFlags Notice msgf (Just perms)

openFileReadWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                                HasDoMock ω, FileAs γ) ⇒
                               𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ
                             → DoMock → μ Handle
openFileReadWriteNoTruncUTF8 =
  openFileUTF8 ReadWriteMode readWriteNoTruncFlags Notice

openFileWriteUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                     MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                     HasDoMock ω, FileAs γ) ⇒
                    𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                  → μ Handle
openFileWriteUTF8 = openFileUTF8 WriteMode writeFlags Notice

openFileWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                       MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                       HasDoMock ω, FileAs γ) ⇒
                      𝕄 (File → 𝕋) → FileMode → μ Handle → γ → DoMock
                    → μ Handle
openFileWriteExUTF8 msgf perms =
  openFileUTF8 WriteMode writeExFlags Notice msgf (Just perms)

openFileWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                            MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                            HasDoMock ω, FileAs γ) ⇒
                           𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                         → μ Handle
openFileWriteNoTruncUTF8 = openFileUTF8 WriteMode writeNoTruncFlags Notice

openFileAppendUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                      MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                      HasDoMock ω, FileAs γ) ⇒
                     𝕄 (File → 𝕋) → 𝕄 FileMode → μ Handle → γ → DoMock
                   → μ Handle
openFileAppendUTF8 = openFileUTF8 AppendMode appendFlags Notice

----------------------------------------

withFile ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
            Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → Severity
         → 𝕄 (File → 𝕋) → 𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withFile enc nlm mode flags sev msgf perms a (review _File_ → fn) io mck =
  let go = MonadIO.File.withFile enc nlm mode flags perms fn io
   in join $ doFile (ѥ go) mode sev msgf a fn mck

----------------------------------------

withFileME ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
              Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
             TextEncoding → NewlineMode → IOMode → OpenFileFlags → Severity
           → 𝕄 (File → 𝕋) → 𝕄 FileMode → μ α → γ → (Handle → ExceptT ε IO α)
           → DoMock → μ α
withFileME enc nlm mode flags sev msgf perms a (review _File_ → fn) io mck =
  let go = MonadIO.File.withFileME enc nlm mode flags perms fn io
   in join $ doFile (ѥ go) mode sev msgf a fn mck

----------------------------------------

withFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
                  Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                 IOMode → OpenFileFlags → Severity → 𝕄 (File → 𝕋) → 𝕄 FileMode
               → μ α → γ → (Handle → IO α) → DoMock → μ α
withFileBinary = withFile char8 noNewlineTranslation

withFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
                Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
               IOMode → OpenFileFlags → Severity → 𝕄 (File → 𝕋) → 𝕄 FileMode
             → μ α → γ → (Handle → IO α) → DoMock → μ α
withFileUTF8 = withFile utf8 nativeNewlineMode

----------------------------------------

withReadFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                      MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                      FileAs γ) ⇒
                     μ α → γ → (Handle → IO α) → DoMock → μ α
withReadFileBinary =
  withFileBinary ReadMode readFlags Informational Nothing Nothing

----------

withReadWriteFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                           MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                           HasDoMock ω, FileAs γ) ⇒
                          𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withReadWriteFileBinary =
  withFileBinary ReadWriteMode readWriteFlags Notice Nothing

----------

withReadWriteExFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                             MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                             HasDoMock ω, FileAs γ) ⇒
                            FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withReadWriteExFileBinary perms =
  withFileBinary ReadWriteMode readWriteExFlags Notice Nothing (Just perms)

----------

withReadWriteNoTruncFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                  MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                                  HasDoMock ω, FileAs γ) ⇒
                                 𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock
                               → μ α
withReadWriteNoTruncFileBinary =
  withFileBinary ReadWriteMode readWriteNoTruncFlags Notice Nothing

----------

withWriteFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                       MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                       HasDoMock ω, FileAs γ) ⇒
                      𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteFileBinary =
  withFileBinary WriteMode writeFlags Notice Nothing

----------

withWriteExFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                         MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                         HasDoMock ω, FileAs γ) ⇒
                        FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteExFileBinary perms =
  withFileBinary WriteMode writeExFlags Notice Nothing (Just perms)

----------

withWriteNoTruncFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                              MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                              HasDoMock ω, FileAs γ) ⇒
                             𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock
                           → μ α
withWriteNoTruncFileBinary =
  withFileBinary WriteMode writeNoTruncFlags Notice Nothing

----------

withAppendFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                       MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                       HasDoMock ω, FileAs γ) ⇒
                      𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withAppendFileBinary =
  withFileBinary AppendMode appendFlags Notice Nothing

----------------------------------------

withReadFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, MonadLog (Log ω) μ,
                    Default ω, HasIOClass ω, HasDoMock ω, FileAs γ) ⇒
                   μ α → γ → (Handle → IO α) → DoMock → μ α
withReadFileUTF8 =
  withFileUTF8 ReadMode readFlags Informational Nothing Nothing

----------

withReadWriteFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                         MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                         HasDoMock ω, FileAs γ) ⇒
                        𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withReadWriteFileUTF8 =
  withFileUTF8 ReadWriteMode readWriteFlags Notice Nothing

----------

withReadWriteExFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                           MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                           HasDoMock ω, FileAs γ) ⇒
                          FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withReadWriteExFileUTF8 perms =
  withFileUTF8 ReadWriteMode readWriteExFlags Notice Nothing (Just perms)

----------

withReadWriteNoTruncFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                                HasDoMock ω, FileAs γ) ⇒
                               𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock
                             → μ α
withReadWriteNoTruncFileUTF8 =
  withFileUTF8 ReadWriteMode readWriteNoTruncFlags Notice Nothing

----------

withWriteFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                     MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                     HasDoMock ω, FileAs γ) ⇒
                    𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteFileUTF8 =
  withFileUTF8 WriteMode writeFlags Notice Nothing

----------

withWriteExFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                       MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                       HasDoMock ω, FileAs γ) ⇒
                      FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteExFileUTF8 perms =
  withFileUTF8 WriteMode writeExFlags Notice Nothing (Just perms)

----------

withWriteNoTruncFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                            MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                            HasDoMock ω, FileAs γ) ⇒
                           𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withWriteNoTruncFileUTF8 =
  withFileUTF8 WriteMode writeNoTruncFlags Notice Nothing

----------

withAppendFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                      MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                      HasDoMock ω, FileAs γ) ⇒
                     𝕄 FileMode → μ α → γ → (Handle → IO α) → DoMock → μ α
withAppendFileUTF8 =
  withFileUTF8 AppendMode appendFlags Notice Nothing

----------------------------------------

{- | Read a file as bytes. -}
readFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                  MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                 μ ByteString → γ → DoMock → μ ByteString
readFileBinary a fn = withReadFileBinary a fn BS.hGetContents

----------

writeFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                   MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                  𝕄 FileMode → γ → ByteString → DoMock → μ ()
writeFileBinary perms fn txt =
  withWriteFileBinary perms (return ()) fn (\ h → BS.hPutStr h txt)

----------

writeNoTruncFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                          MonadLog (Log ω) μ, Default ω, HasDoMock ω,
                          HasIOClass ω) ⇒
                         𝕄 FileMode → γ → ByteString → DoMock → μ ()
writeNoTruncFileBinary perms fn txt =
  withWriteNoTruncFileBinary perms (return ()) fn (\ h → BS.hPutStr h txt)

----------

writeExFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                     MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                    FileMode → γ → ByteString → DoMock → μ ()
writeExFileBinary perms fn txt =
  withWriteExFileBinary perms (return ()) fn (\ h → BS.hPutStr h txt)

----------

appendFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                   MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                  𝕄 FileMode → γ → ByteString → DoMock → μ ()
appendFileBinary perms fn txt =
  withAppendFileBinary perms (return ()) fn (\ h → BS.hPutStr h txt)

----------------------------------------

readFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
               μ ByteString → γ → DoMock → μ ByteString
readFileUTF8 a fn = withReadFileUTF8 a fn BS.hGetContents

----------

readFileUTF8Lenient ∷ (AsIOError ε, MonadError ε μ, MonadIO μ, FileAs γ,
                       MonadLog (Log ω) μ, Default ω, HasDoMock ω,
                       HasIOClass ω) ⇒
                      μ 𝕋 → γ → DoMock → μ 𝕋
readFileUTF8Lenient a fn =
  withReadFileUTF8 a fn (decodeUtf8With lenientDecode ⩺ BS.hGetContents)

----------

writeFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                 MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                𝕄 FileMode → γ → ByteString → DoMock → μ ()
writeFileUTF8 perms fn txt =
  withWriteFileUTF8 perms (return ()) fn (\ h → BS.hPutStr h txt)

----------

writeNoTruncFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                        MonadLog (Log ω) μ, Default ω, HasDoMock ω,
                        HasIOClass ω) ⇒
                       𝕄 FileMode → γ → ByteString → DoMock → μ ()
writeNoTruncFileUTF8 perms fn txt =
  withWriteNoTruncFileUTF8 perms (return ()) fn (\ h → BS.hPutStr h txt)

----------

writeExFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                   MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                  FileMode → γ → ByteString → DoMock → μ ()
writeExFileUTF8 perms fn txt =
  withWriteExFileUTF8 perms (return ()) fn (\ h → BS.hPutStr h txt)

----------

appendFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ,
                  MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                 𝕄 FileMode → γ → ByteString → DoMock → μ ()
appendFileUTF8 perms fn txt =
  withAppendFileUTF8 perms (return ()) fn (\ h → BS.hPutStr h txt)

----------------------------------------

{- | Work over a file, accumulating results, line-by-line. -}
fileFoldLinesUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                     MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                     α → (α → 𝕋 → IO α) → μ α → γ → DoMock → μ α
fileFoldLinesUTF8 a io w fn = withReadFileUTF8 w fn $ fileFoldLinesH a io

-- that's all, folks! ----------------------------------------------------------
