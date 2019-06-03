module Blick.Database where

import           Blick.Database.Secret as Secret
import           Blick.Types           (SecretBody)
import qualified Data.Acid             as Acid
import qualified Data.UUID             as UUID
import qualified Data.UUID.V4          as UUID.V4


lookupSecret :: Acid.AcidState Secret.Database -> Secret.Key -> IO (Maybe SecretBody)
lookupSecret acidDb secretId = do
  Acid.query acidDb (Get secretId)


createSecret :: Acid.AcidState Secret.Database -> SecretBody -> IO (String)
createSecret acidDb secret = do
  secretId <- genSecretId
  Acid.update acidDb (Set secretId secret)
  return secretId


genSecretId :: IO String
genSecretId = do
  uuid <- UUID.V4.nextRandom
  return $ UUID.toString uuid
