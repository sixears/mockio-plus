{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO.File
  ( readFile, withFile, withWriteFile, writeFile)
where

-- base --------------------------------

import qualified  System.IO

import Data.Function           ( ($) )
import Data.Maybe              ( Maybe( Just, Nothing ), maybe )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import System.IO               ( Handle, IO, IOMode( WriteMode ), stdout )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( toString, toText )

-- fpath -------------------------------

import FPath.AbsFile    ( AbsFile )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational, Notice ) )

import MockIO  ( DoMock( NoMock ) )

-- mockio-log --------------------------

import MockIO.Log      ( MockIOClass, mkIOL )
import MockIO.IOClass  ( IOClass( IORead, IOWrite ) )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text     ( Text, unpack )
import Data.Text.IO  ( hPutStr )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

--------------------------------------------------------------------------------

withFile ∷ MonadIO μ ⇒ AbsFile → IOMode → (Handle → IO ω) → μ ω
withFile fn mode = liftIO ∘ System.IO.withFile (toString fn) mode

----------------------------------------

{- | With a file opened for writing, or `stdout` if no file provided. -}
withWriteFile ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒
                DoMock → α → Maybe AbsFile → (Handle → IO α) → μ α
withWriteFile mck a fn io = do
  let fname  = maybe "-STDOUT-" toText fn
      logmsg = [fmtT|write %t|] fname
  case fn of
    Nothing  → liftIO $ io stdout
    Just wfn → mkIOL Notice IOWrite logmsg a (withFile wfn WriteMode io) mck

----------------------------------------

writeFile ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒
            DoMock → Maybe AbsFile → Text → μ ()
writeFile mck fnY txt = withWriteFile mck () fnY (\ h → hPutStr h txt)

----------------------------------------

readFile ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ Text → μ Text
readFile fn = let logmsg = [fmtT|read %t|] fn
                  result = Data.Text.IO.readFile (unpack fn)
               in mkIOL Informational IORead logmsg "" result NoMock

-- that's all, folks! ----------------------------------------------------------
