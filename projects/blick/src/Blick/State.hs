{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- TODO: Store? Database? Some other name...
module Blick.State where

import           Blick.Types          (SecretBody)
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import qualified Data.Acid            as Acid
import qualified Data.Map             as M
import           Data.Maybe           (Maybe)
import qualified Data.SafeCopy        as SafeCopy
import           Data.Typeable        (Typeable)

type Key = String
type Value = SecretBody

data AppState
  = AppState (M.Map Key Value)
  deriving (Show, Typeable)

$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''AppState)


setSecret :: Key -> Value -> Acid.Update AppState ()
setSecret k v = do
  AppState state <- get
  put $ AppState $ M.insert k v state


getSecret :: Key -> Acid.Query AppState (Maybe Value)
getSecret k = do
  AppState secrets <- ask
  pure $ M.lookup k secrets


$(Acid.makeAcidic ''AppState ['setSecret, 'getSecret])


initialAppState :: AppState
initialAppState =
  AppState M.empty


loadAcidState :: IO (Acid.AcidState AppState)
loadAcidState =
  Acid.openLocalStateFrom "acid" initialAppState
