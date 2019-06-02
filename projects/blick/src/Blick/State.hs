{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- TODO: Store? Database? Some other name...
module Blick.State where

import           Blick.Types          (SecretBody)
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (put)
import qualified Data.Acid            as Acid
import qualified Data.Map             as M
import           Data.Maybe           (Maybe)
import qualified Data.SafeCopy        as SafeCopy
import           Data.Typeable        (Typeable)

data AppState
  = AppState
  { secrets :: M.Map String SecretBody
  } deriving (Show, Typeable)

$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''AppState)


setSecret :: AppState -> String -> SecretBody -> Acid.Update AppState ()
setSecret state k v =
  put $ state { secrets = updatedSecrets }

  where
    origSecrets =
      secrets state

    updatedSecrets =
      M.insert k v origSecrets

getSecret :: String -> Acid.Query AppState (Maybe SecretBody)
getSecret k = do
  AppState { secrets = s } <- ask
  pure $ M.lookup k s

$(Acid.makeAcidic ''AppState ['setSecret, 'getSecret])


initialAppState :: AppState
initialAppState =
  AppState { secrets = M.empty }


loadAcidState :: IO (Acid.AcidState AppState)
loadAcidState =
  Acid.openLocalStateFrom "acid" initialAppState
