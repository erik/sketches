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


-- | This is pretty gross, but the idea is that since the frontend is
-- responsible for routing, we never actually send a 404 from the
-- backend. The frontend always receives the same barebones html and
-- does the routing / rendering.
serveSinglePageApp :: Wai.Application
serveSinglePageApp _req respond =
  respond $
        Wai.responseLBS
        status200
        [("Content-Type", "text/html")]
        baseHtml


-- TODO: Probably switch to something like Blaze HTML
baseHtml =
  "<!doctype html>\n\
  \<head>\n\
  \  <meta charset=\"utf-8\">\n\
  \  <title>blick</title>\n\
  \  <link rel=\"stylesheet\" href=\"/static/main.css\">\n\
  \</head>\n\
  \<body>\n\
  \  <div id=\"app\"></div>\n\
  \  <script src=\"/static/main.js\"></script>\n\
  \  <script src=\"/static/ports.js\"></script>\n\
  \  <script>\n\
  \    const app = Elm.Main.init({node: document.getElementById(\"app\")});\n\
  \    registerPorts(app);\n\
  \  </script>\n\
  \</body>"
