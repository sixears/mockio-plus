{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO.Directory
  ( chdir, inDir, lsdir, lsdir', mkdir, mkpath, nuke )
where

import Base1T

-- base --------------------------------

import System.IO               ( FilePath )
import System.Posix.Types      ( FileMode )

-- directory ---------------------------

import System.Directory  ( listDirectory )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadCatch )

-- fpath -------------------------------

import FPath.AppendableFPath   ( AppendableFPath, AppendableFPathD
                               , AppendableFPathF, (⫻) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Dir               ( DirAs )
import FPath.DirType           ( DirType )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.Parent            ( HasParentMay )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.RelFile           ( RelFile )
import FPath.ToDir             ( ToDir )

-- fstat -------------------------------

import FStat  ( FStat )

-- log-plus ----------------------------

import Log  ( Log, logIO )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Warning ) )

-- mockio ------------------------------

import MockIO  ( DoMock( DoMock ) )

-- mockio-log --------------------------

import MockIO.Log      ( HasDoMock, doMock, logResult, mkIOL, mkIOLME, mkIOLMER)
import MockIO.IOClass  ( HasIOClass, IOClass( IORead, IOWrite ), ioClass )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( IOError )

-- monadio-plus ------------------------

import qualified  MonadIO.Directory
import MonadIO.FStat  ( pathTypes )

-- safe --------------------------------

import Safe  ( succSafe )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.FStat  ( lstats )

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

----------------------------------------

_lstdr ∷ ∀ ε δ ω μ .
          (MonadIO μ, DirAs δ,
           AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
           HasIOClass ω, HasDoMock ω, Default ω, MonadLog (Log ω) μ) ⇒
         Severity → [FilePath] → δ → DoMock → μ [FilePath]
_lstdr sev mck_val d do_mock = do
  let vmsg = 𝕵 $ \ fps → [ [fmt|'%T'|] fp | fp ← fps ]
      lstd = asIOError $ listDirectory (d ⫥ filepath)
  mkIOLMER sev IORead ([fmt|lstdr: '%T'|] d) vmsg mck_val lstd do_mock

----------

{-| List a directory's files & subdirs, along with their stat results. -}
lsdir ∷ ∀ ε ε' ρ ω μ .
        (MonadIO μ,
         ToDir ρ, DirAs (AppendableFPathD ρ),
         AppendableFPath ρ, AppendableFPathF ρ ~ RelFile, Printable (DirType ρ),
         AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
         AsIOError ε', Printable ε',
         HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
        Severity
      → ([(ρ, FStat)], [(DirType ρ, FStat)], [(ρ, ε')])
      → AppendableFPathD ρ
      → DoMock
      → μ ([(ρ, FStat)], [(DirType ρ, FStat)], [(ρ, ε')])

lsdir sev mck_val d do_mock = do
  let msg = [fmtT|lsdir '%T'|] d
      log_attr = def & ioClass ⊢ IORead & doMock ⊢ do_mock
      vmsg = 𝕵 $ \(fs,ds,es) → ю [ [ [fmtT|%T|] f | (f,_fstat) ← fs ]
                                 , [ [fmtT|%T|] d' | (d',_fstat) ← ds ]
                                 , [ [fmtT|«%T» %T|] p e | (p,e) ← es ]
                                   ]

      sev' = succSafe sev

      go = do
        fns ← _lstdr sev' [] d do_mock
        xs ← sequence $ (fmap (d ⫻) ∘ parse @RelFile) ⊳ fns
        (foldr pathTypes ([],[],[]) ⩺ \ d' → lstats sev' 𝕹 d' do_mock) xs

  r ← mkIOLME sev IORead msg mck_val go  do_mock
  logResult sev log_attr do_mock msg vmsg (𝕽 r)

--------------------

{-| Simplified version of `lsdir`, where the mock value is all empties; and
    the errors are logged with `warnIO` but not returned. -}

lsdir' ∷ ∀ ε ρ ω μ .
         (MonadIO μ,
          ToDir ρ, DirAs (AppendableFPathD ρ),
          AppendableFPath ρ, AppendableFPathF ρ ~ RelFile, Printable(DirType ρ),
          AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ,HasCallStack,
          HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
         Severity → AppendableFPathD ρ → DoMock
       → μ ([(ρ, FStat)], [(DirType ρ, FStat)])

lsdir' sev d do_mock = do
  (fs,ds,es) ← lsdir @_ @IOError sev ([],[],[]) d do_mock
  forM_ es $ \ (f,e) →
    -- like `MockIO.Log.warnIO`; but not a fixed MockIOClass to Log
    mkIOL Warning def ([fmtT|lsdir: '%T' ! %T|] f e) () (return ()) do_mock
  return (fs,ds)

-- that's all, folks! ----------------------------------------------------------
