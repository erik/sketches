{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Blick.Database.Secret where

import           Blick.Types          (SecretBody)
import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State  as MonadState
import qualified Data.Acid            as Acid
import qualified Data.Map             as M
import           Data.Maybe           (Maybe)
import qualified Data.SafeCopy        as SafeCopy
import           Data.Typeable        (Typeable)

type Key = String
type Value = SecretBody

data Database
  = Database !(M.Map Key Value)
  deriving (Show, Typeable)

$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''Database)


set :: Key -> Value -> Acid.Update Database ()
set k v = do
  Database secrets <- MonadState.get
  MonadState.put $ Database $ M.insert k v secrets


get :: Key -> Acid.Query Database (Maybe Value)
get k = do
  Database secrets <- MonadReader.ask
  pure $ M.lookup k secrets


$(Acid.makeAcidic ''Database ['set, 'get])


initialDatabase :: Database
initialDatabase =
  Database M.empty

{-| Load acid-state database from "./acid/", creating it if it didn't
  already exist.
-}
loadAcidState :: IO (Acid.AcidState Database)
loadAcidState =
  Acid.openLocalStateFrom "acid" initialDatabase
