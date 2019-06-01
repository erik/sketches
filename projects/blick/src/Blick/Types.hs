{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Blick.Types where

import           Data.Aeson   as Aeson
import           Data.Maybe   (Maybe)
import           Data.Time    (UTCTime)
import qualified GHC.Generics as GHC

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
