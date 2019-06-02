module Blick.Server where

import           Servant

import qualified Blick.API   as API
import           Blick.Types (SecretBody (..))

server :: Servant.Server API.API
server
  = getSecret
  :<|> createSecret


getSecret :: String -> Servant.Handler SecretBody
getSecret _id =
  return SecretBody {
      blob = "foo"
    , creation_date = read "2011-11-19 18:28:52.607875 UTC"
    , expiration_date = Nothing
  }

createSecret :: SecretBody -> Servant.Handler String
createSecret _body =
  return "ok"
