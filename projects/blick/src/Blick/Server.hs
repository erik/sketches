module Blick.Server where

import           Servant

import qualified Blick.API            as API
import           Blick.Context        (AppCtx (..), AppM)
import qualified Blick.Secret         as Secret
import           Blick.Types          (SecretBody (..))
import           Control.Monad.Reader (asks, liftIO)
import qualified Data.Acid            as Acid

import qualified Data.UUID            as UUID
import qualified Data.UUID.V4         as UUID.V4

server :: Servant.ServerT API.API AppM
server
  = getSecret
  :<|> createSecret


getSecret :: String -> AppM SecretBody
getSecret secretId = do
  state <- asks _getSecretDb
  secret <- liftIO $ Acid.query state (Secret.Get secretId)

  case secret of
    Nothing ->
      -- TODO: Descriptive error
      throwError err404

    Just s ->
      return s


createSecret :: SecretBody -> AppM String
createSecret body = do
  state <- asks _getSecretDb
  secretId <- liftIO genSecretId
  _ <- liftIO $ Acid.update state (Secret.Set secretId body)

  return secretId


genSecretId :: IO String
genSecretId = do
  uuid <- UUID.V4.nextRandom
  return $ UUID.toString uuid
