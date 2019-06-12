module Blick where

import qualified Blick.API                as API
import           Blick.Config             (Config (..), configFromEnv)
import           Blick.Context            (AppCtx (..))
import qualified Blick.Database.Secret
import           Blick.Server             (server)
import qualified Control.Exception        as Exception
import           Control.Monad.Reader     (runReaderT)
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger       as Logger
import qualified Servant


main :: IO ()
main = do
  config <- configFromEnv
  secretDb <- Blick.Database.Secret.loadAcidState

  putStrLn $ "Starting server: " ++ (configBaseURL config)

  let context = AppCtx config secretDb

  Exception.catch
    (startServer context)
    (\ Exception.UserInterrupt -> putStrLn "stop.")

makeApplication :: AppCtx -> Wai.Application
makeApplication ctx =
  Servant.serve API.combined serverWithContext

  where
    serverWithContext = Servant.hoistServer API.combined (flip runReaderT ctx) server


startServer :: AppCtx -> IO ()
startServer context = do
  Logger.withStdoutLogger $ \logger -> do
    let config = _getConfig context
        settings =
          Warp.setPort (configPort config) $
          Warp.setLogger logger $
          Warp.defaultSettings
        app = makeApplication context

    Warp.runSettings settings app
