{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO.Directory
  ( chdir, inDir, mkdir, mkpath, nuke )
where

-- base --------------------------------

import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( (&) )
import GHC.Stack               ( HasCallStack )
import System.IO               ( IO )
import System.Posix.Types      ( FileMode )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode  ( (≡) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadCatch )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath )
import FPath.Dir         ( DirAs )
import FPath.DirType     ( DirType )
import FPath.Parent      ( HasParentMay )

-- log-plus ----------------------------

import Log  ( Log, logIO )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO  ( DoMock( DoMock ) )

-- mockio-log --------------------------

import MockIO.Log      ( HasDoMock, doMock, mkIOLME )
import MockIO.IOClass  ( HasIOClass, IOClass( IORead, IOWrite ), ioClass )

-- monadio-error -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import qualified  MonadIO.Directory

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊢) )
import Data.MoreUnicode.Text   ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

--------------------------------------------------------------------------------

{- | Change working directory. -}
chdir ∷ ∀ ε δ ω μ .
        (MonadIO μ,
         AsIOError ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         DirAs δ) ⇒
        Severity -- ^ logging level
      → δ        -- ^ directory to change to
      → DoMock   -- ^ whether to mock
      → μ ()
chdir sev d =
  mkIOLME sev IORead ([fmtT|chdir %T|] d) () (MonadIO.Directory.chdir d)

----------------------------------------

{- | Perform IO with the dir *temporarily* changed to a given directory. -}
inDir ∷ ∀ ε δ α ω μ .
        (MonadIO μ, DirAs δ,AsIOError ε, MonadError ε μ, HasCallStack,
         HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
        Severity       -- ^ logging level
      → δ              -- ^ directory to work in
      → α              -- ^ value to return if mocked
      → ExceptT ε IO α -- ^ IO to perform in the given directory
      → DoMock         -- ^ whether to mock
      → μ α            -- ^ value returned by IO (or mock value)

inDir sev d mck_val io mck = do
  let log = [fmtT|indir %T|] d
      rlog ∷ 𝕋
      rlog = if DoMock ≡ mck then "(indir RETURN)" else "indir RETURN"
  r ← mkIOLME sev IORead log mck_val (MonadIO.Directory.inDir d io) mck
  logIO sev (def & ioClass ⊢ IORead & doMock ⊢ mck) rlog
  return r

----------------------------------------

{- | Forcibly remove a file or directory (including any descendents). -}
nuke ∷ ∀ ε ρ ω μ .
        (MonadIO μ,
         AsIOError ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         AsFilePath ρ, Printable ρ) ⇒
        Severity -- ^ logging level
      → ρ        -- ^ file/dir to remove
      → DoMock   -- ^ whether to mock
      → μ ()
nuke sev d =
  mkIOLME sev IOWrite ([fmtT|nuke %T|] d) () (MonadIO.Directory.nuke d)

----------------------------------------

{- | Create a (single) directory.  Will error if the directory already exists
     (either as a directory or a file), or the parent directory does not exist
     or is not writable by the current user.
 -}
mkdir ∷ ∀ ε δ ω μ .
        (MonadIO μ,
         AsIOError ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         DirAs δ) ⇒
        Severity -- ^ logging level
      → δ        -- ^ directory to create
      → FileMode -- ^ permissions for the directory
      → DoMock   -- ^ whether to mock
      → μ ()
mkdir sev d p =
  mkIOLME sev IOWrite ([fmtT|mkdir %T|] d) () (MonadIO.Directory.mkdir d p)

----------------------------------------

mkpath ∷ ∀ ε δ ω μ .
        (MonadIO μ, MonadCatch μ,
         AsIOError ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         DirAs δ, HasParentMay δ, HasParentMay (DirType δ),
         DirType δ ~ DirType (DirType δ), δ ~ DirType δ) ⇒
        Severity -- ^ logging level
      → δ        -- ^ directory to create
      → FileMode -- ^ permissions for the directory
      → DoMock   -- ^ whether to mock
      → μ ()
mkpath sev d p =
  mkIOLME sev IOWrite ([fmtT|mkpath %T|] d) () (MonadIO.Directory.mkpath d p)

-- that's all, folks! ----------------------------------------------------------
