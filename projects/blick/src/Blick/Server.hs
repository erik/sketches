module Blick.Server where

import           Servant

import qualified Blick.API            as API
import           Blick.Context        (AppCtx (..), AppM)
import qualified Blick.Database       as Database
import           Blick.Types          (CreateSecretResponse (..),
                                       SecretBody (..))
import           Control.Monad.Reader (asks, liftIO)


server :: Servant.ServerT API.API AppM
server
  = getSecret
  :<|> createSecret


getSecret :: String -> AppM SecretBody
getSecret secretId = do
  db <- asks _getSecretDb
  secret <- liftIO $ Database.lookupSecret db secretId

  case secret of
    Nothing ->
      -- TODO: Descriptive error
      throwError err404

    Just s ->
      return s


createSecret :: SecretBody -> AppM CreateSecretResponse
createSecret body = do
  db <- asks _getSecretDb
  secretId <- liftIO $ Database.createSecret db body

  return $ CreateSecretResponse { secretId = secretId }
