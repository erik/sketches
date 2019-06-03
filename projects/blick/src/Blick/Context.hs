-- TODO: maybe Server.Context?
module Blick.Context where

import           Blick.Config          (Config (..))
import           Blick.Database.Secret as Secret
import           Control.Monad.Reader  (ReaderT)
import qualified Data.Acid             as Acid
import qualified Servant

type AppM = ReaderT AppCtx Servant.Handler

data AppCtx
  = AppCtx
  { _getConfig   :: Config
  , _getSecretDb :: Acid.AcidState Secret.Database
  }
