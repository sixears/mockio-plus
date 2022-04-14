module MockIO.Process.MLCmdSpec
  ( HasMLCmdSpec( cmdspec, cmdrw, mlCmdSpec, mock, mock_value, severity )
  , MLCmdSpec, ToCmdSpec( toCmdSpec ), ToMLCmdSpec( toMLCmdSpec )
  , mlMkCmd, mlMkCmd', mkMLCmd, mkMLCmd', mkMLCmdR, mkMLCmdR', mkMLCmdW
  , mkMLCmdW'
  )
where

import Base1T

-- base --------------------------------

import Data.Tuple  ( uncurry )

-- env-plus ----------------------------

import Env        ( getEnvironment )
import Env.Types  ( EnvModFrag, runEnvMod', ҙ )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity( Informational, Notice ) )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock( NoMock ) )

-- monadio-plus ------------------------

import MonadIO.Process.CmdSpec        ( CmdSpec, HasCmdArgs( cmdArgs )
                                      , HasExpExitSig( expExitSig )
                                      , HasExpExitVal( expExitVal )
                                      , HasCmdExe( cmdExe )
                                      , HasCmdSpec( cmdSpec, env )
                                      , mkCmd, mkCmd'
                                      )
import MonadIO.Process.ExitStatus     ( ExitStatus( ExitVal ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.Process.CmdRW          ( CmdRW( CmdR, CmdW ) )
import MockIO.Process.OutputDefault  ( OutputDefault( outDef ) )

--------------------------------------------------------------------------------

data MLCmdSpec ξ = MLCmdSpec { _severity   ∷ Severity
                             , _cmdrw      ∷ CmdRW
                             , _mock       ∷ DoMock
                             , _cmdspec    ∷ CmdSpec
                             , _mock_value ∷ (ExitStatus, ξ)
                             }

class HasMLCmdSpec α ξ | α → ξ where
  mlCmdSpec  ∷ Lens' α (MLCmdSpec ξ)
  severity   ∷ Lens' α Severity
  severity   = mlCmdSpec ∘ severity
  cmdrw      ∷ Lens' α CmdRW
  cmdrw      = mlCmdSpec ∘ cmdrw
  mock_value ∷ Lens' α (ExitStatus, ξ)
  mock_value = mlCmdSpec ∘ mock_value
  cmdspec    ∷ Lens' α CmdSpec
  cmdspec    = mlCmdSpec ∘ cmdspec
  mock       ∷ Lens' α DoMock
  mock       = mlCmdSpec ∘ mock

instance HasMLCmdSpec (MLCmdSpec ξ) ξ where
  mlCmdSpec  = id
  severity   = lens _severity   (\ sp s  → sp { _severity = s })
  cmdrw      = lens _cmdrw      (\ sp c  → sp { _cmdrw = c })
  mock_value = lens _mock_value (\ sp mv → sp { _mock_value = mv })
  cmdspec    = lens _cmdspec    (\ sp c  → sp { _cmdspec = c })
  mock       = lens _mock       (\ sp m  → sp { _mock = m })

instance HasCmdExe (MLCmdSpec ξ) where
  cmdExe = cmdspec ∘ cmdExe

instance HasCmdArgs (MLCmdSpec ξ) where
  cmdArgs = cmdspec ∘ cmdArgs

instance HasCmdSpec (MLCmdSpec ξ) where
  cmdSpec = cmdspec

instance HasExpExitVal (MLCmdSpec ξ) where
  expExitVal = cmdspec ∘ expExitVal

instance HasExpExitSig (MLCmdSpec ξ) where
  expExitSig = cmdspec ∘ expExitSig

class ToCmdSpec α where
  toCmdSpec ∷ MonadIO μ ⇒ α → μ (CmdSpec, [𝕋])

instance ToCmdSpec CmdSpec where
  toCmdSpec = return ∘ (,[])

-- cmdspec & envmod messages
instance ToCmdSpec (CmdSpec,[𝕋]) where
  toCmdSpec = return

instance ToCmdSpec (AbsFile, [𝕋]) where
  toCmdSpec = return ∘ (,[]) ∘ uncurry mkCmd'

instance ToCmdSpec (AbsFile, [𝕋], [EnvModFrag]) where
  toCmdSpec (p,as,es) = do
    (e,env_mod_msgs) ← runEnvMod' (ҙ es) ⊳ getEnvironment
    return (mkCmd' p as & env ⊢ 𝕵 e, env_mod_msgs)

------------------------------------------------------------

{-| Make an `MLCmdSpec` from a `ToCmdSpec`, using an `OutputDefault` and
    post-modifying the spec (this is for creating `ToMLCmdSpec` instances. -}
mkMLCmdSpec ∷ ∀ ξ χ μ .
       (MonadIO μ, ToCmdSpec χ, OutputDefault ξ) ⇒
       χ → (MLCmdSpec ξ → MLCmdSpec ξ) → μ (MLCmdSpec ξ)
mkMLCmdSpec x f =
  mlMkCmd' Informational CmdR x NoMock ≫ \ m → return $ m & mlCmdSpec ⊧ f

----------------------------------------

class ToMLCmdSpec α ξ where
  toMLCmdSpec ∷ ∀ μ . (MonadIO μ, OutputDefault ξ) ⇒ α → μ (MLCmdSpec ξ, [𝕋])

----------

instance ToMLCmdSpec (MLCmdSpec ξ) ξ where
  toMLCmdSpec = return ∘ (,[])

--------------------

instance ToMLCmdSpec (AbsFile, [𝕋], [EnvModFrag],
                      MLCmdSpec ξ → MLCmdSpec ξ) ξ where
  toMLCmdSpec (a,as,es,f) = do
    (e,ms) ← runEnvMod' (ҙ es) ⊳ getEnvironment
    (\ x → (x & env ⊢ 𝕵 e, ms)) ⊳ mkMLCmdSpec (a,as) f

--------------------

instance ToMLCmdSpec (AbsFile, [𝕋], MLCmdSpec ξ → MLCmdSpec ξ) ξ where
  toMLCmdSpec (a,as,f) = toMLCmdSpec (a,as,[]∷[EnvModFrag] ,f)

--------------------

instance ToMLCmdSpec (AbsFile, [𝕋], [EnvModFrag]) ξ where
  toMLCmdSpec (a,as,es) = toMLCmdSpec (a,as,es,id ∷ MLCmdSpec ξ → MLCmdSpec ξ)

--------------------

instance ToMLCmdSpec (AbsFile, [𝕋]) ξ where
  toMLCmdSpec (a,as) =
    toMLCmdSpec (a,as,[]∷[EnvModFrag],id ∷ MLCmdSpec ξ → MLCmdSpec ξ)

------------------------------------------------------------

{- | Create an `MLCmdSpec` using something that can be converted to a `CmdSpec`.
     Exit code is 0 and no signals are expected. -}
mlMkCmd ∷ (MonadIO μ, ToCmdSpec χ) ⇒
          Severity → CmdRW → χ → ξ → DoMock → μ (MLCmdSpec ξ)
mlMkCmd sev rw cspec x mck = do
  (cmd_spec,_) ← toCmdSpec cspec
  return $ MLCmdSpec sev rw mck cmd_spec (ExitVal 0, x)

{- | Create an `MLCmdSpec` using something that can be converted to a `CmdSpec`,
     and using `OutputDefault` for mock values. -}
mlMkCmd' ∷ (MonadIO μ, ToCmdSpec χ, OutputDefault ξ) ⇒
           Severity → CmdRW → χ → DoMock → μ (MLCmdSpec ξ)
mlMkCmd' sev rw cspec mck = mlMkCmd sev rw cspec outDef mck

{- | Create an `MLCmdSpec` using `OutputDefault` for mock values.  Exit code is
     0 and no signals are expected. -}
mkMLCmd ∷ OutputDefault ξ ⇒
          Severity → CmdRW → AbsFile → [𝕋] → DoMock → MLCmdSpec ξ
mkMLCmd sev rw exe args mck =
   MLCmdSpec sev rw mck (mkCmd exe args) (ExitVal 0, outDef)

{- | Create an `MLCmdSpec` using `OutputDefault` for mock values; set '/' for
     the cwd, use an empty environment and create a process group. -}
mkMLCmd' ∷ OutputDefault ξ ⇒
          Severity → CmdRW → AbsFile → [𝕋] → DoMock → MLCmdSpec ξ
mkMLCmd' sev rw exe args mck =
   MLCmdSpec sev rw mck (mkCmd' exe args) (ExitVal 0, outDef)

{- | Create an `MLCmdSpec` for a "passive" external cmd, with default output
     mock values (logging severity `Informational`). -}
mkMLCmdR ∷ OutputDefault ξ ⇒ AbsFile → [𝕋] → DoMock → MLCmdSpec ξ
mkMLCmdR = mkMLCmd Informational CmdR

{- | Like `mkMLCmdR`, but based on `mkMLCmd'`. -}
mkMLCmdR' ∷ OutputDefault ξ ⇒ AbsFile → [𝕋] → DoMock → MLCmdSpec ξ
mkMLCmdR' = mkMLCmd' Informational CmdR

{- | Create an `MLCmdSpec` for an "active" external cmd, with default output
     mock values (logging severity `Notice`). -}
mkMLCmdW ∷ OutputDefault ξ ⇒ AbsFile → [𝕋] → DoMock → MLCmdSpec ξ
mkMLCmdW = mkMLCmd Notice CmdW

{- | Like `mkMLCmdW`, but based on `mkMLCmd'`. -}
mkMLCmdW' ∷ OutputDefault ξ ⇒ AbsFile → [𝕋] → DoMock → MLCmdSpec ξ
mkMLCmdW' = mkMLCmd' Notice CmdW

-- that's all, folks! ----------------------------------------------------------
