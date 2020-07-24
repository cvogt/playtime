module Playtime.SaveLoad where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed (makeRelativeToProject, strToExp)
import My.IO
import My.Prelude
import Playtime.Types

saveLocation :: FilePath
saveLocation = $(makeRelativeToProject "savegame.json" >>= strToExp)

-- FIXME: this hard codes against OneTimeEffects in EngineState. We probably don't want to hard code loading and saving
saveMay :: ToJSON gs => EngineState -> gs -> IO ()
saveMay es gs = do
  let EngineState {gsActions} = es
  when (OneTimeEffect Save `setMember` gsActions) $ writeFile saveLocation $ BSL.toStrict $ encode gs

loadMay :: FromJSON gs => EngineState -> IO (Maybe gs)
loadMay es = do
  let EngineState {gsActions} = es
  if OneTimeEffect Load `setMember` gsActions
    then do
      either fail pure . eitherDecode . BSL.fromStrict =<< readFile saveLocation
    else pure Nothing
