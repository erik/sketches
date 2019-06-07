{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Blick.Types where

import           Data.Aeson    as Aeson
import           Data.Maybe    (Maybe)
import qualified Data.SafeCopy as SafeCopy
import           Data.Time     (UTCTime)
import qualified GHC.Generics  as GHC

data SecretBody = SecretBody {
    blob           :: String
  , creationDate   :: UTCTime
  , expirationDate :: Maybe UTCTime
  } deriving ( Aeson.FromJSON
             , Aeson.ToJSON
             , GHC.Generic
             , Eq
             , Show
             )

-- For use with acid-state
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''SecretBody)

data CreateSecretResponse = CreateSecretResponse {
    secretId :: String
  } deriving (Aeson.ToJSON, GHC.Generic, Show)
