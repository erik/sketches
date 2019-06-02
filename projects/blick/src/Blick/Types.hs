{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Blick.Types where

import           Data.Aeson    as Aeson
import           Data.Maybe    (Maybe)
import qualified Data.SafeCopy as SafeCopy
import           Data.Time     (UTCTime)
import qualified GHC.Generics  as GHC

data SecretBody = SecretBody {
    blob            :: String
  , creation_date   :: UTCTime
  , expiration_date :: Maybe UTCTime
  } deriving ( Aeson.FromJSON
             , Aeson.ToJSON
             , GHC.Generic
             , Eq
             , Show
             )

$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''SecretBody)
