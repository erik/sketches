-- TODO: maybe Server.Context?
module Blick.Context where

import           Blick.Config         (Config (..))
import           Control.Monad.Reader (ReaderT)
import qualified Servant

type AppM = ReaderT AppCtx Servant.Handler

data AppCtx
  = AppCtx
  { _getConfig   :: Config
  , _getDatabase :: String
  }
