module Blick.Server where

import           Servant

import qualified Blick.API            as API
import           Blick.Context        (AppCtx (..), AppM)
import           Blick.Types          (SecretBody (..))
import           Control.Monad.Reader (asks)

server :: Servant.ServerT API.API AppM
server
  = getSecret
  :<|> createSecret


getSecret :: String -> AppM SecretBody
getSecret secretId = do
  db <- asks _getDatabase

  pure SecretBody {
      blob = "foo:" ++ secretId
    , creation_date = read "2011-11-19 18:28:52.607875 UTC"
    , expiration_date = Nothing
  }

createSecret :: SecretBody -> AppM String
createSecret _body =
  return "ok"
