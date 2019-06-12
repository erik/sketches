{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Blick.Server where

import           Servant
import qualified Servant.Server.StaticFiles as Static

import qualified Blick.API                  as API
import           Blick.Context              (AppCtx (..), AppM)
import qualified Blick.Database             as Database
import           Blick.Types                (CreateSecretResponse (..),
                                             SecretBody (..))
import           Control.Monad.Reader       (asks, liftIO)
import           Network.HTTP.Types         (status200)
import           Network.Wai                as Wai


apiServer :: Servant.ServerT API.API AppM
apiServer =
       getSecret
  :<|> createSecret

server :: Servant.ServerT API.Combined AppM
server =
       apiServer
  :<|> (Static.serveDirectoryWebApp "./assets")
  :<|> (Tagged serveSinglePageApp)


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


-- TODO: Return a legitimate HTML body
serveSinglePageApp :: Wai.Application
serveSinglePageApp _req respond = respond $
        Wai.responseLBS
        status200
        [("Content-Type", "text/html")]
        "<h1>this is totally HTML.</h1>"
