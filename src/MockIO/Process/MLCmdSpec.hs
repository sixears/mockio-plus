module MockIO.Process.MLCmdSpec
  ( HasMLCmdSpec( cmdspec, cmdrw, mock, mock_value, severity ), MLCmdSpec
  , mkMLCmd, mkMLCmd', mkMLCmdR, mkMLCmdR', mkMLCmdW, mkMLCmdW' )
where


-- base --------------------------------

import Data.Function  ( id )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity( Informational, Notice ) )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock )

-- monadio-plus ------------------------

import MonadIO.Process.CmdSpec        ( CmdSpec, HasCmdArgs( cmdArgs )
                                      , HasExpExitSig( expExitSig )
                                      , HasExpExitVal( expExitVal )
                                      , HasCmdExe( cmdExe )
                                      , HasCmdSpec( cmdSpec )
                                      , mkCmd, mkCmd'
                                      )
import MonadIO.Process.ExitStatus     ( ExitStatus( ExitVal ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Text  ( 𝕋 )

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

{- | Create an `MLCmdSpec` using `OutputDefault` for mock values.  Expected exit
     code is 0 and no signals are expected. -}
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
