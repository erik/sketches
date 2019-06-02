{-# LANGUAGE OverloadedStrings #-}

module Blick where

import qualified Blick.API                as API
import           Blick.Config             (Config (..), configFromEnv)
import qualified Blick.Server             as Server
import qualified Control.Exception        as Exception
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger       as Logger
import qualified Servant


main :: IO ()
main = do
  config <- configFromEnv
  putStrLn $ "Starting server: " ++ (configBaseURL config)

  Exception.catch
    (startServer config)
    (\ Exception.UserInterrupt -> putStrLn "stop.")


application :: Wai.Application
application =
  Servant.serve API.api Server.server


startServer :: Config -> IO ()
startServer config =
  Logger.withStdoutLogger $ \logger -> do
     let settings =
           Warp.setPort (configPort config) $
           Warp.setLogger logger $
           Warp.defaultSettings

     Warp.runSettings settings application
