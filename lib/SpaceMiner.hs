module SpaceMiner where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import Game
import Graphics
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
-- import Music
import My.Extra
import My.IO
import My.Prelude
import SpaceMiner.ConcurrentState
import SpaceMiner.Debug
import SpaceMiner.Debug.Vty
import SpaceMiner.GL
import SpaceMiner.GLFW
import SpaceMiner.Types
import System.Exit (exitSuccess)

main :: IO ()
main = do
  -- basic configuration
  let logicDim = Dimensions {width = 320, height = 240}
  let scale = ScaleInt 3

  -- initialization
  igs <- makeInitialGameState logicDim <$> getSystemTime
  cs@ConcurrentState {csTotalLoopTime, csRenderLoopTime} <- makeInitialConcurrentState igs

  --void $ forkIO $ forever $ playMusic
  void $ forkDebugTerminal cs

  -- game state loop
  void $ forkIO $ do
    flip unfoldM_ igs $ \old_gs -> do
      events <- fetchEvents cs
      let new_gs = applyEventsToGameState events old_gs
      saveMay new_gs
      final_gs <- fromMaybe new_gs <$> loadMay new_gs
      sendGameState cs final_gs
      sendSpritePlacements cs $ computeSpritePlacements final_gs
      pure $ if gameExitRequested final_gs then Nothing else Just final_gs
    exitSuccess

  -- open gl rendering loop
  withGLFW logicDim scale "SpaceMiner" $ \window -> do
    textures <- loadTextures
    startCaptureEvents window logicDim cs
    forever $ trackTime csTotalLoopTime $ do
      GLFW.pollEvents -- before takeMVar frees game loop for another run
      receiveSpritePlacements cs >>= trackTime csRenderLoopTime . renderGame textures window logicDim
  where
    saveLocation = $(makeRelativeToProject "savegame.json" >>= strToExp)
    saveMay (GameState GenericGameState {gsActions} _ persistentGameState) =
      when (OneTimeEffect Save `setMember` gsActions) $ writeFile saveLocation $ BSL.toStrict $ encode $ persistentGameState
    loadMay gameState@(GameState GenericGameState {gsActions} _ _) =
      if OneTimeEffect Load `setMember` gsActions
        then do
          npgs <- either fail pure . eitherDecode . BSL.fromStrict =<< readFile saveLocation
          pure $ Just gameState {gsPersistentGameState = npgs}
        else pure Nothing
