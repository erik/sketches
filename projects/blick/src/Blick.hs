module Blick where

import qualified Blick.API                as API
import qualified Blick.Server             as Server
import qualified Control.Exception        as Exception
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger       as Logger
import qualified Servant


main :: IO ()
main = do
  let port = 8080 :: Int
  putStrLn $ "Starting server: http://localhost:" ++ show port

  Exception.catch
    (startServer port)
    (\ Exception.UserInterrupt -> putStrLn "stop.")


application :: Wai.Application
application =
  Servant.serve API.api Server.server


startServer :: Int -> IO ()
startServer port =
  Logger.withStdoutLogger $ \logger -> do
     let settings =
           Warp.setPort port $
           Warp.setLogger logger $
           Warp.defaultSettings

     Warp.runSettings settings application
