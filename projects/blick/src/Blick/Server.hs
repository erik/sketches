module Blick.Server where

import           Servant

import qualified Blick.API   as API
import           Blick.Types (SecretBody)
import qualified Blick.Types as Types
import           Data.Time   (UTCTime)

server :: Servant.Server API.API
server
  = getSecret
  :<|> createSecret


-- TODO: Kill awful `Types` prefix.
getSecret :: String -> Servant.Handler SecretBody
getSecret _id =
  return $ Types.SecretBody {
      Types.blob = "foo"
    , Types.creation_date = (read "2011-11-19 18:28:52.607875 UTC")::UTCTime
    , Types.expiration_date = Nothing
  }

createSecret :: SecretBody -> Servant.Handler String
createSecret _body =
  return $ "ok"
