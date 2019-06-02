module Blick where

import           Blick.API                (api)
import           Blick.Config             (Config (..), configFromEnv)
import           Blick.Context            (AppCtx (..))
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
  putStrLn $ "Starting server: " ++ (configBaseURL config)

  -- TODO: pull this out somewhere
  let context = AppCtx config "foo"

  Exception.catch
    (startServer context)
    (\ Exception.UserInterrupt -> putStrLn "stop.")

makeApplication :: AppCtx -> Wai.Application
makeApplication ctx =
  Servant.serve api serverWithContext

  where
    serverWithContext = Servant.hoistServer api (flip runReaderT ctx) server


startServer :: AppCtx -> IO ()
startServer context =
  Logger.withStdoutLogger $ \logger -> do
    let config = _getConfig context
        settings =
          Warp.setPort (configPort config) $
          Warp.setLogger logger $
          Warp.defaultSettings
        app = makeApplication context

    Warp.runSettings settings app
