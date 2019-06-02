module Blick.Server where

import           Servant

import qualified Blick.API            as API
import           Blick.Context        (AppCtx (..), AppM)
import qualified Blick.State          as State
import           Blick.Types          (SecretBody (..))
import           Control.Monad.Reader (asks, liftIO)
import qualified Data.Acid            as Acid

server :: Servant.ServerT API.API AppM
server
  = getSecret
  :<|> createSecret


getSecret :: String -> AppM SecretBody
getSecret secretId = do
  state <- asks _getState
  secret <- liftIO $ Acid.query state (State.GetSecret secretId)

  case secret of
    Nothing ->
      throwError err404

    Just s ->
      return s


createSecret :: SecretBody -> AppM String
createSecret _body =
  return "ok"
