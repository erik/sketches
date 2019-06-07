{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Blick.API where

import           Servant.API as Servant

import           Blick.Types (CreateSecretResponse, SecretBody)
import qualified Data.Proxy  as Proxy


type API
  = GetSecret
  :<|> CreateSecret

-- TODO: understand this
api :: Proxy.Proxy API
api = Proxy.Proxy

-- GET /secret/:id
type GetSecret = "secret"
  :> Capture "id" String
  :> Get '[Servant.JSON] SecretBody

-- POST /secret
type CreateSecret = "secret"
  :> ReqBody '[Servant.JSON] SecretBody
  :> Post '[Servant.JSON] CreateSecretResponse
