module MockIO.File
  ( AccessMode(..), FExists(..),
    access, chmod, fexists, fexists', lfexists, lfexists'
  , fileWritable, isWritableDir, isWritableFile
  , lstat, stat
  , rename
  , unlink
  , writable

  , fileFoldLinesUTF8
  )
where

-- base --------------------------------

import Control.Applicative     ( pure )
import Data.Function           ( ($) )
import Data.Maybe              ( maybe )
import Control.Monad.IO.Class  ( MonadIO )
import GHC.Stack               ( HasCallStack )
import System.IO               ( IO )
import System.Posix.Types      ( FileMode )
import Text.Show               ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( Default )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath )
import FPath.File        ( File, FileAs )
import FPath.Dir         ( DirAs )

-- fstat -------------------------------

import FStat  ( FStat )

-- lens --------------------------------

import Control.Lens  ( view )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO  ( DoMock )

-- mockio-log --------------------------

import MockIO.Log      ( HasDoMock, mkIOLMER )
import MockIO.IOClass  ( HasIOClass, IOClass( IORead, IOWrite ) )

-- monadio-error -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import qualified  MonadIO.File
import MonadIO.File         ( AccessMode(..), FExists(..), fileFoldLinesH )
import MonadIO.NamedHandle  ( handle )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )
import Control.Monad.Trans   ( lift )

-- text --------------------------------

import Data.Text  ( lines, pack )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.OpenFile  ( HEncoding( UTF8 ), FileOpenMode( FileR ), withFile )

--------------------------------------------------------------------------------

{- | Work over a file, accumulating results, line-by-line. -}
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

access ∷ ∀ ε ρ ω μ .
         (MonadIO μ,
          AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
          AsFilePath ρ, Printable ρ) ⇒
         Severity → AccessMode → 𝕄 𝔹 → ρ → DoMock → μ (𝕄 𝔹)
access sev amode mock_value fn = do
  let msg = [fmt|accss %T %w|] fn amode
      vmsg = 𝕵 $ maybe ["Nothing"] (pure ∘ pack ∘ show)
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.access amode fn)

----------------------------------------

_stat ∷ ∀ ε ρ ω μ .
        (MonadIO μ, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         AsFilePath ρ, Printable ρ) ⇒
        (ρ → ExceptT ε IO (𝕄 FStat))
      → Severity → 𝕄 FStat → ρ → DoMock → μ (𝕄 FStat)
_stat s sev mock_value fn mck =
  let msg  = [fmt|stat  %T|] fn
      vmsg = 𝕵 $ maybe ["Nothing"] (lines ∘ toText)
   in mkIOLMER sev IORead msg vmsg mock_value (s fn) mck

--------------------

stat ∷ ∀ ε ρ ω μ .
       (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
        MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
        AsFilePath ρ, Printable ρ) ⇒
       Severity → 𝕄 FStat → ρ → DoMock → μ (𝕄 FStat)
stat = _stat MonadIO.File.stat

----------

lstat ∷ ∀ ε ρ ω μ .
        (MonadIO μ,
         AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         AsFilePath ρ, Printable ρ) ⇒
        Severity → 𝕄 FStat → ρ → DoMock → μ (𝕄 FStat)
lstat = _stat MonadIO.File.lstat

----------------------------------------

{- | Simple shortcut for file (or directory) is writable by this user; `Nothing`
     is returned if file does not exist. -}
writable ∷ ∀ ε ρ ω μ .
           (MonadIO μ,
            AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
            AsFilePath ρ, Printable ρ) ⇒
           Severity → 𝕄 𝔹 → ρ → DoMock → μ (𝕄 𝔹)
writable sev = access sev ACCESS_W

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

{- | Is `f` an extant writable file? -}
isWritableFile ∷ ∀ ε γ ω μ .
                 (MonadIO μ,
                  AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                  MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                  FileAs γ, Printable γ) ⇒
                 Severity → 𝕄 𝕋 → γ → DoMock → μ (𝕄 𝕋)

isWritableFile sev mock_value fn =
  let msg = [fmt|isWrF %T|] fn
      vmsg = 𝕵 $ maybe ["file is writable"] pure
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.isWritableFile fn)

----------------------------------------

{- | Is `f` an extant writable directory? -}
isWritableDir ∷ ∀ ε γ ω μ .
                (MonadIO μ,
                 AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                 MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                 DirAs γ, Printable γ) ⇒
                Severity → 𝕄 𝕋 → γ → DoMock → μ (𝕄 𝕋)

isWritableDir sev mock_value fn =
  let msg = [fmt|isWrD %T|] fn
      vmsg = 𝕵 $ maybe ["file is writable"] pure
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.isWritableDir fn)

----------------------------------------

{- | Test that the given path is a writable (by this user) *file*, or does not
     exist but is in a directory that is writable & executable by this user.
     In case of not writable, some error text is returned to say why.
 -}
fileWritable ∷ ∀ ε γ ω μ .
               (MonadIO μ,
                AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
                MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                FileAs γ, Printable γ) ⇒
               Severity → 𝕄 𝕋 → γ → DoMock → μ (𝕄 𝕋)
fileWritable sev mock_value fn =
  let msg = [fmt|filWr %T|] fn
      vmsg = 𝕵 $ maybe ["file is (potentially) writable"] pure
   in mkIOLMER sev IORead msg vmsg mock_value (MonadIO.File.fileWritable fn)


{- | See `MonadIO.File.rename` -}
rename ∷ ∀ ε γ δ ω μ .
         (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω, FileAs γ,
          FileAs δ, Printable γ, Printable δ) ⇒
         Severity → γ → δ → DoMock → μ ()
rename sev from to =
  let msg = [fmt|renam '%T' → '%T'|] from to
   in mkIOLMER sev IOWrite msg 𝕹 () (MonadIO.File.rename from to)

-- that's all, folks! ----------------------------------------------------------
