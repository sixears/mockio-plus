{-# LANGUAGE UnicodeSyntax #-}
module MockIO.File
  ( AccessMode(..)
  , FExists(..)
  , chmod
  , fexists
  , fexists'
  , fileFoldLinesUTF8
  , lfexists
  , lfexists'
  , readlink
  , rename
  , resolvelink
  , unlink
  ) where

import Base1T

-- base --------------------------------

import System.Posix.Types ( FileMode )

-- fpath -------------------------------

import FPath.Abs              ( Abs(AbsD) )
import FPath.AbsDir           ( root )
import FPath.AbsFile          ( AbsFile )
import FPath.AsFilePath       ( AsFilePath )
import FPath.Error.FPathError ( AsFPathError )
import FPath.File             ( File, FileAs )
import FPath.ToDir            ( toDir )
import FPath.ToFile           ( toFileY )

-- fstat -------------------------------

import FStat ( FileType(Directory, SymbolicLink), ftype )

-- lens --------------------------------

import Control.Lens ( view )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO ( DoMock(NoMock), HasDoMock(doMock) )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass(ioClass), IOClass(IORead, IOWrite) )
import MockIO.Log     ( logResult, mkIOLME, mkIOLMER )

-- monadio-error -----------------------

import MonadError.IO ( ioThrow )

-- monadio-plus ------------------------

import MonadIO.File        ( AccessMode(..), FExists(..), fileFoldLinesH )
import MonadIO.File qualified
import MonadIO.NamedHandle ( handle )

-- mtl ---------------------------------

import Control.Monad.Trans ( lift )

-- text --------------------------------

import Data.Text ( pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.FStat    ( lstat )
import MockIO.OpenFile ( FileOpenMode(FileR), HEncoding(UTF8), withFile )

--------------------------------------------------------------------------------

{- | Work over a file, accumulating results, line-by-line.
     `sev` is the severity to log messages.  `msgf` is an optional logging
     message for opening the file (if `Nothing`, then a default is used).
     `a` is the initial value of the fold; `io' is the folding function; `fn` is
     the file to read.  `w` is the mock value.
 -}
fileFoldLinesUTF8 ∷ ∀ ε α γ ω μ .
                    (MonadIO μ, FileAs γ,
                     AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                     MonadLog (Log ω) μ, Default ω, HasDoMock ω, HasIOClass ω) ⇒
                    Severity → 𝕄 (File → 𝕋) → α → (α → 𝕋 → IO α) → μ α → γ
                  → DoMock → μ α
fileFoldLinesUTF8 sev msgf a io w fn mck =
--   withReadFileUTF8 w fn $ fileFoldLinesH a io
  withFile sev msgf UTF8 FileR w fn
           (lift ∘ fileFoldLinesH a io ∘ view handle) mck

----------------------------------------

fexists ∷ ∀ ε ρ ω μ .
          (MonadIO μ,
           AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
           MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
           AsFilePath ρ, Printable ρ) ⇒
          Severity → FExists → ρ → DoMock → μ FExists
fexists sev mock_value fn = do
  let msg = [fmt|fxist %T|] fn
      vmsg = 𝕵 $ (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.fexists fn)

--------------------

fexists' ∷ ∀ ε ρ ω μ .
           (MonadIO μ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
            AsFilePath ρ, Printable ρ) ⇒
           Severity → FExists → ρ → DoMock → μ FExists
fexists' sev mock_value fn = do
  let msg = [fmt|fxst' %T|] fn
      vmsg = 𝕵 $ (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.fexists' fn)

--------------------

lfexists ∷ ∀ ε ρ ω μ .
           (MonadIO μ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
            AsFilePath ρ, Printable ρ) ⇒
           Severity → FExists → ρ → DoMock → μ FExists
lfexists sev mock_value fn = do
  let msg = [fmt|lfxst %T|] fn
      vmsg = 𝕵 $ (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.lfexists fn)

--------------------

lfexists' ∷ ∀ ε ρ ω μ .
            (MonadIO μ,
             AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
             MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
             AsFilePath ρ, Printable ρ) ⇒
            Severity → FExists → ρ → DoMock → μ FExists
lfexists' sev mock_value fn = do
  let msg = [fmt|lfxt' %T|] fn
      vmsg = 𝕵 $ (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.lfexists' fn)

----------------------------------------

chmod ∷ ∀ ε ρ ω μ .
        (MonadIO μ,
         AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         AsFilePath ρ, Printable ρ) ⇒
        Severity → FileMode → ρ → DoMock → μ ()
chmod sev perms fn =
  let msg = [fmt|chmod %T %04o|] fn perms
   in mkIOLMER sev IOWrite msg 𝕹 () (MonadIO.File.chmod perms fn)

----------------------------------------

unlink ∷ ∀ ε γ ω μ .
         (MonadIO μ,
          AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
          FileAs γ, Printable γ) ⇒
         Severity → γ → DoMock → μ ()
unlink sev fn =
  mkIOLMER sev IOWrite ([fmt|unlnk %T|] fn) 𝕹 () (MonadIO.File.unlink fn)

----------------------------------------

{- | See `MonadIO.File.rename` -}
rename ∷ ∀ ε γ δ ω μ .
         (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω, FileAs γ,
          FileAs δ, Printable γ, Printable δ) ⇒
         Severity → γ → δ → DoMock → μ ()
rename sev from to =
  let msg = [fmt|renam '%T' → '%T'|] from to
   in mkIOLMER sev IOWrite msg 𝕹 () (MonadIO.File.rename from to)

----------------------------------------

{- | See `MonadIO.File.readlink` -}
readlink ∷ ∀ ε ω μ .
           (MonadIO μ, HasCallStack,
            AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
           Severity → Abs → AbsFile → DoMock → μ Abs
readlink sev mock_value fp =
  let msg = [fmt|rdlnk '%T'|] fp
      vmsg ∷ 𝕄 (Abs → [𝕋])
      vmsg = 𝕵 $ pure ∘ [fmt|rdlnk '%T' → '%T'|] fp
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.readlink fp)

----------------------------------------

resolvelink' ∷ ∀ ε ω μ .
              (MonadIO μ, HasCallStack,
               AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Severity → [AbsFile] → AbsFile → μ Abs

resolvelink' sev prior fp = do
  when (fp ∈ prior) $ ioThrow ([fmtT|readlink cycle detected: %L|] prior)
  r ← readlink sev (AbsD root) fp NoMock
  ftype ⊳⊳ lstat sev 𝕹 r NoMock ≫ \ case
    𝕵 SymbolicLink → case toFileY r of
                       𝕵 r' → resolvelink' sev (fp:prior) r'
                       -- this should never happen; toFileY only fails
                       -- / or ./, and neither can ever be a symlink
                       𝕹    → ioThrow $ [fmtT|?eh?: '%T' is a symlink!?|] r
    𝕵 Directory    → return $ AbsD (toDir r)
    _              → return r

----------

{- | See `MonadIO.File.resolvelink` -}
resolvelink ∷ ∀ ε ω μ .
              (MonadIO μ, HasCallStack,
               AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Severity → Abs → AbsFile → DoMock → μ Abs

resolvelink sev mock_value fp do_mock = do
  let msg = [fmtT|rsvlk '%T'|] fp
      vmsg ∷ 𝕄 (Abs → [𝕋])
      vmsg = 𝕵 $ pure ∘ [fmt|rsvlk '%T' → '%T'|] fp
      log_attr = def & ioClass ⊢ IORead & doMock ⊢ do_mock
  r ← mkIOLME sev IORead msg mock_value (resolvelink' sev [] fp) do_mock
  logResult sev log_attr do_mock msg vmsg (𝕽 r)

-- that's all, folks! ----------------------------------------------------------
