module Playtime.Setup where

import Codec.Picture (DynamicImage)
import Data.Aeson
import GHC.Err (error)
import My.Extra
import My.IO
import My.Prelude
import Playtime.Debug
import Playtime.GL
import Playtime.LiveCode
import Playtime.Types

-- FIXME: this file needs cleanup. it implements strict texture loading, but also lazy loading,
--        which is obviously non-sensical to have both for the same textures.
--        we may eventually want to strict load certain textures and lazy load others.

wireEngineConfig ::
  forall a gs.
  (Ord a, Show a, ToJSON gs, FromJSON gs) =>
  Dim ->
  Scale ->
  Maybe LiveCodeState ->
  ((a -> Texture) -> EngineState -> gs -> Event -> IO gs) ->
  ((a -> Texture) -> EngineState -> gs -> [Sprite]) ->
  (a -> IO DynamicImage) ->
  [a] ->
  gs ->
  IO EngineConfig
wireEngineConfig ecDim ecScale liveCodeState stepGameState visualize loadTx allTextures initialGameState = do
  recoveredGameState <- for liveCodeState startLiveCode
  gameStateMVar <- newMVar $ fromMaybe initialGameState $ join recoveredGameState
  loadedTexturesMVar <- newMVar mempty
  let ecStepGameState = \es event -> do
        modifyMVar_ gameStateMVar $ \old_gs -> do
          loadedTextures <-
            modifyMVar loadedTexturesMVar $ \loadedTextures' -> do
              if not $ mapNull loadedTextures'
                then pure $ dupe loadedTextures'
                else do
                  lt' <- for allTextures $ \i -> (i,) <$> (either fail pure =<< runExceptT . loadTexture =<< loadTx i)
                  pure $ dupe $ foldl (\m (k, v) -> mapInsert k v m) loadedTextures' lt'
          new_gs <- stepGameState (lt loadedTextures) es old_gs event
          for_ liveCodeState $ flip liveCodeSwitch new_gs
          pure new_gs
      ecVisualize = \es -> do
        loadedTextures <- readMVar loadedTexturesMVar
        visualizations <- visualize (lt loadedTextures) es <$> readMVar gameStateMVar
        -- NOTE: resurrect this when implementing dynamically loaded textures
        -- updateTextureCache loadedTexturesMVar visualizations loadTx
        pure visualizations
      ecCheckIfContinue = pure . not . gameExitRequested
      ecGameDebugInfo = \EngineState {..} -> debugPrint <$> readMVar gameStateMVar
  pure $ EngineConfig {..}

lt :: (Ord a, Show a) => Map a Texture -> a -> Texture
lt loadedTextures t =
  fromMaybe (error $ "error loading texture " <> show t <> ", did you forget putting it into all_textures?") $
    mapLookup t loadedTextures

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
