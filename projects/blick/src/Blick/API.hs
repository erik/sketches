{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Blick.API where

import           Servant.API                as Servant

import           Blick.Types                (CreateSecretResponse, SecretBody)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Proxy                 (Proxy (..))


type API =
         GetSecret
    :<|> CreateSecret

type Combined =
         ("api" :> API)
    :<|> ("static" :> Raw)
    :<|> SinglePageApp

combined :: Proxy Combined
combined = Proxy

-- GET /secret/:id
type GetSecret = "secret"
    :> Capture "id" String
    :> Get '[Servant.JSON] SecretBody

-- POST /secret
type CreateSecret = "secret"
    :> ReqBody '[Servant.JSON] SecretBody
    :> Post '[Servant.JSON] CreateSecretResponse

type SinglePageApp =
    Raw
    -- TODO: Should hook this up:
    -- :> Get '[HTML] String
