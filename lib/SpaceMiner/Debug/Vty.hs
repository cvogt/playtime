module SpaceMiner.Debug.Vty where

import qualified Data.Map as Map
import GHC.Float (int2Double)
import GHC.Real ((/), round)
import qualified Graphics.Vty as Vty
import My.Extra
import My.IO
import My.Prelude
import SpaceMiner.ConcurrentState
import SpaceMiner.Types
import SpaceMiner.Util

forkDebugTerminal :: ConcurrentState -> IO ThreadId
forkDebugTerminal ConcurrentState {..} = do
  -- FIXME: cursor stays hidden after termination
  cfg <- Vty.standardIOConfig
  vty <- Vty.mkVty cfg
  flip forkFinally (\_ -> Vty.shutdown vty) $ do
    flip iterateM_ (0, 0, 0, 0) $ \(oldAvgGameLoopTime, oldAvgTexturePlacementTime, oldAvgRenderLoopTime, oldAvgTotalLoopTime) -> do
      gs <- readMVar csGameState
      let GenericGameState {..} = get gs
          PersistentGameState {..} = get gs
      gameLoopTimes <- modifyMVar csGameLoopTime $ \t -> pure ([], t)
      texturePlacementTimes <- modifyMVar csSpritePlacementTime $ \t -> pure ([], t)
      renderLoopTimes <- modifyMVar csRenderLoopTime $ \t -> pure ([], t)
      totalLoopTimes <- modifyMVar csTotalLoopTime $ \t -> pure ([], t)
      let newAvgGameLoopTime = if not $ null gameLoopTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> gameLoopTimes) else oldAvgGameLoopTime
          newAvgTexturePlacementTime = if not $ null texturePlacementTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> texturePlacementTimes) else oldAvgTexturePlacementTime
          newAvgRenderLoopTime = if not $ null renderLoopTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> renderLoopTimes) else oldAvgRenderLoopTime
          newAvgTotalLoopTime = if not $ null totalLoopTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> totalLoopTimes) else oldAvgTotalLoopTime
          Pos x y = gsCursorPos
          Pos x' y' = gsMainCharacterPosition
      Vty.update vty $ Vty.picForImage $ foldl1 (Vty.<->) $
        Vty.string (Vty.defAttr `Vty.withForeColor` Vty.white)
          <$> ( "fps: " <> show newAvgTotalLoopTime
                  :| [ "1/renderLoopTime: " <> show newAvgRenderLoopTime,
                       "1/texturePlacementTime: " <> show newAvgTexturePlacementTime,
                       "1/gameLoopTime: " <> show newAvgGameLoopTime,
                       "opengl pos: " <> show (x, y),
                       "main char: " <> show (x', y'),
                       "keys: " <> show gsKeysPressed,
                       "collisions: " <> show gsCollisions,
                       "last places sprite location: " <> show gsLastPlacement,
                       "sprite count floor: " <> show (Map.size $ unBoard gsFloor),
                       "sprite count room: " <> show (Map.size $ unBoard gsRoom)
                     ]
              )

      threadDelay $ 500 * 1000 -- FIXME: changing this to 100 * make process freeze on exit
      pure (newAvgGameLoopTime, newAvgTexturePlacementTime, newAvgRenderLoopTime, newAvgTotalLoopTime)
