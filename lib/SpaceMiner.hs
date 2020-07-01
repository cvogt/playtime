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
    flip unfoldM_ igs $ \ogs -> do
      events <- fetchEvents cs
      let tgs = appleEventsToGameState events ogs
      gsLoaded <- saveOrLoadIfRequested tgs
      let ngs = fromMaybe tgs gsLoaded
      sendGameState cs ngs
      sendSpritePlacements cs $ computeSpritePlacements ngs
      pure $ if gameExitRequested ngs then Nothing else Just ngs
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
    saveOrLoadIfRequested gameState@(GameState GenericGameState {gsRequestedSaveGame, gsRequestedLoadGame} _ persistentGameState) =
      if gsRequestedLoadGame
        then do
          npgs <- either fail pure . eitherDecode . BSL.fromStrict =<< readFile saveLocation
          pure $ Just gameState {gsPersistentGameState = npgs}
        else do
          when gsRequestedSaveGame $ writeFile saveLocation $ BSL.toStrict $ encode $ persistentGameState
          pure Nothing
