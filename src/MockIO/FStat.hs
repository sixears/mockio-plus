module MockIO.FStat
  ( access, fileWritable, isWritableDir, isWritableFile, lstat, lstat', lstats
  , stat, stat', stats, writable )
where

import Base1T

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath( filepath ) )
import FPath.File        ( FileAs )
import FPath.Dir         ( DirAs )

-- fstat -------------------------------

import FStat  ( FStat )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO  ( DoMock, HasDoMock )

-- mockio-log --------------------------

import MockIO.Log      ( mkIOLMER )
import MockIO.IOClass  ( HasIOClass, IOClass( IORead ) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( unsquashNoSuchThing' )

-- monadio-plus ------------------------

import qualified  MonadIO.File
import MonadIO.File  ( AccessMode(..) )

-- text --------------------------------

import Data.Text  ( lines, pack )

--------------------------------------------------------------------------------

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

{-| See `MonadIO.FStat.lstat` -}
stat ∷ ∀ ε ρ ω μ .
       (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
        MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
        AsFilePath ρ, Printable ρ) ⇒
       Severity → 𝕄 FStat → ρ → DoMock → μ (𝕄 FStat)
stat = _stat MonadIO.File.stat

{-| Like `stat`, but a missing file raises a @DoesNotExistError@ -}
stat' ∷ ∀ ε ρ ω μ .
         (MonadIO μ, AsFilePath ρ, Printable ρ,
          AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
         Severity → 𝕄 FStat → ρ → DoMock → μ FStat
stat' sev mck_val fp do_mock =
  let lst r = stat sev mck_val r do_mock
   in unsquashNoSuchThing' lst ("stat" ∷ 𝕋) (fp ⫥ filepath)

--------------------

{-| See `MonadIO.FStat.lstat` -}
lstat ∷ ∀ ε ρ ω μ .
        (MonadIO μ,
         AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
         AsFilePath ρ, Printable ρ) ⇒
        Severity → 𝕄 FStat → ρ → DoMock → μ (𝕄 FStat)
lstat = _stat MonadIO.File.lstat

----------

{-| Like `lstat`, but a missing file raises a @DoesNotExistError@ -}
lstat' ∷ ∀ ε ρ ω μ .
         (MonadIO μ, AsFilePath ρ, Printable ρ,
          AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
         Severity → 𝕄 FStat → ρ → DoMock → μ FStat
lstat' sev mck_val fp do_mock =
  let lst r = lstat sev mck_val r do_mock
   in unsquashNoSuchThing' lst ("lstat" ∷ 𝕋) (fp ⫥ filepath)

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

----------------------------------------

{-| Pair a list of files with their stat outputs. -}
stats ∷ ∀ ε ρ ψ η ω μ .
        (MonadIO μ, AsFilePath ρ, Printable ρ, Traversable ψ,
         Printable ε, AsIOError ε, MonadError ε η, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
        Severity → 𝕄 FStat → ψ ρ → DoMock → μ (ψ (ρ, η FStat))
stats sev mck_val fns do_mock =
  sequence $ fmap (\ fn → (fn,) ⊳ ѥ (stat' sev mck_val fn do_mock)) fns

--------------------

{-| Like `stats`, but using `lstat` -}
lstats ∷ ∀ ε ρ ψ η ω μ .
        (MonadIO μ, AsFilePath ρ, Printable ρ, Traversable ψ,
         Printable ε, AsIOError ε, MonadError ε η, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
        Severity → 𝕄 FStat → ψ ρ → DoMock → μ (ψ (ρ, η FStat))
lstats sev mck_val fns do_mock =
  sequence $ fmap (\ fn → (fn,) ⊳ ѥ (lstat' sev mck_val fn do_mock)) fns

-- that's all, folks! ----------------------------------------------------------
