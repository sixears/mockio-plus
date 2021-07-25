module MockIO.Process.CmdRW
  ( CmdRW(..), ioc )
where

-- mockio-log --------------------------

import MockIO.IOClass  ( IOClass( IOCmdR, IOCmdW ) )

--------------------------------------------------------------------------------

{- | Marker for whether a cmd merely observes the environment - e.g., lists a
     file - or updates the environment, e.g., sets an env.var. -}
data CmdRW = CmdR | CmdW

ioc ∷ CmdRW → IOClass
ioc CmdR = IOCmdR
ioc CmdW = IOCmdW

-- that's all, folks! ----------------------------------------------------------
