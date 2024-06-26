module Playtime.Wiring where

import Codec.Picture (DynamicImage)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Data.Aeson
import GHC.Err (error)
import My.IO
import My.Prelude
import Playtime.Debug
import Playtime.EngineConfig
import Playtime.EngineState
import Playtime.Event
import Playtime.GL
import Playtime.Geometry
import Playtime.LiveCode
import Playtime.Texture

wireEngineConfig ::
  forall a gs.
  (Ord a, Show a, ToJSON gs, FromJSON gs) =>
  ((a -> Texture) -> IO gs) ->
  ((a -> Texture) -> EngineState -> gs -> Event -> IO gs) ->
  ((a -> Texture) -> EngineState -> gs -> [Sprite]) ->
  Dim ->
  Double ->
  Maybe LiveCodeState ->
  (a -> IO DynamicImage) ->
  [a] ->
  IO EngineConfig
wireEngineConfig makeInitialGameState stepGameState visualize ecDim ecScale liveCodeState loadTx allTextures = do
  gameStateMVar <- newEmptyMVar
  texturesMVar <- newMVar mempty
  wireEngineConfig' gameStateMVar texturesMVar makeInitialGameState stepGameState visualize ecDim ecScale liveCodeState loadTx allTextures

wireEngineConfig' ::
  forall a gs.
  (Ord a, Show a, ToJSON gs, FromJSON gs) =>
  MVar gs ->
  MVar (Map a Texture) ->
  ((a -> Texture) -> IO gs) ->
  ((a -> Texture) -> EngineState -> gs -> Event -> IO gs) ->
  ((a -> Texture) -> EngineState -> gs -> [Sprite]) ->
  Dim ->
  Double ->
  Maybe LiveCodeState ->
  (a -> IO DynamicImage) ->
  [a] ->
  IO EngineConfig
wireEngineConfig' gameStateMVar texturesMVar makeInitialGameState stepGameState visualize ecDim ecScale liveCodeState loadTx allTextures = do
  recoveredGameState <- for liveCodeState startLiveCode
  pure $
    EngineConfig
      { ecInitialize = putMVar gameStateMVar =<< case join recoveredGameState of
          Just gs -> pure gs
          Nothing -> makeInitialGameState =<< readTextures allTextures loadTx texturesMVar,
        ecStepGameState = \es event -> do
          modifyMVar_ gameStateMVar $ \old_gs -> do
            textures <- readTextures allTextures loadTx texturesMVar
            new_gs <- stepGameState textures es old_gs event
            for_ liveCodeState $ flip liveCodeSwitch new_gs
            pure new_gs,
        ecVisualize = \es -> do
          textures <- readTextures allTextures loadTx texturesMVar
          visualize textures es <$> readMVar gameStateMVar,
        ecDim = ecDim,
        ecScale = ecScale,
        ecCheckIfContinue = pure . not . gameExitRequested,
        ecGameDebugInfo = \EngineState {} -> debugPrint <$> readMVar gameStateMVar
      }

readTextures :: (Ord a, Show a) => [a] -> (a -> IO DynamicImage) -> MVar (Map a Texture) -> IO (a -> Texture)
readTextures allTextures loadTx texturesMVar = do
  textures' <- readMVar texturesMVar
  textures <-
    if null textures'
      then do
        textures'' <- loadTextures
        void $ swapMVar texturesMVar textures''
        pure textures''
      else pure textures'
  pure $ \t ->
    fromMaybe (error $ "error loading texture " <> show t <> ", did you forget putting it into all_textures?") $
      mapLookup t textures
  where
    loadTextures = do
      lt' <- for allTextures $ \i -> (i,) <$> (either fail pure =<< runExceptT . loadTexture =<< loadTx i)
      pure $ foldl (\m (k, v) -> mapInsert k v m) mempty lt'

-- NOTE: resurrect this when implementing dynamically loaded textures
-- updateTextureCache loadedTexturesMVar visualizations loadTx
-- updateTextureCache :: Ord a => MVar (Map a Texture) -> [Sprite] -> (a -> IO DynamicImage) -> IO ()
-- updateTextureCache loadedTexturesMVar visualizations f' =
--   modifyMVar loadedTexturesMVar $ \loadedTextures -> do
--     let f (acc, loadedTextures') = \(Sprite area t) -> case t of
--           DynamicSprite ref ->
--             case mapLookup ref loadedTextures' of
--               Nothing -> do
--                 texture <- either fail pure =<< (runExceptT . loadTexture) =<< f' ref
--                 pure (TexturePlacements texture area : acc, mapInsert ref texture loadedTextures')
--               Just texture -> pure (TexturePlacements texture area : acc, loadedTextures')
--           s@Rectangle{} -> pure (s : acc, loadedTextures')
--     swap <$> foldlM f ([], loadedTextures) visualizations
