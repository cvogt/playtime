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

wireEngineConfig ::
  forall a gs.
  (Ord a, Show a, Enum a, Bounded a, ToJSON gs, FromJSON gs) =>
  Dimensions ->
  Scale ->
  LiveCodeState ->
  ((a -> Texture) -> EngineState -> gs -> Event -> IO gs) ->
  ((a -> Texture) -> EngineState -> gs -> [TexturePlacements a]) ->
  (a -> IO DynamicImage) ->
  gs ->
  IO EngineConfig
wireEngineConfig ecDim ecScale liveCodeState stepGameState visualize loadTx initialGameState = do
  recoveredGameState <- startLiveCode liveCodeState
  gameStateMVar <- newMVar $ fromMaybe initialGameState recoveredGameState
  loadedTexturesMVar <- newMVar mempty
  let allTextures :: [a]
      allTextures = enumFrom (minBound :: a) -- this produces a list of all constructors of an enum ADT
      ecStepGameState = \es event -> do
        modifyMVar_ gameStateMVar $ \old_gs -> do
          loadedTextures <-
            modifyMVar loadedTexturesMVar $ \loadedTextures' -> do
              if not $ mapNull loadedTextures'
                then pure $ dupe loadedTextures'
                else do
                  lt' <- for allTextures $ \i -> (i,) <$> (either fail pure =<< runExceptT . loadTexture =<< loadTx i)
                  pure $ dupe $ foldl (\m (k, v) -> mapInsert k v m) loadedTextures' lt'
          new_gs <- stepGameState (lt loadedTextures) es old_gs event
          liveCodeSwitch liveCodeState new_gs
          pure new_gs
      ecVisualize = \es -> do
        loadedTextures <- readMVar loadedTexturesMVar
        visualizations <- visualize (lt loadedTextures) es <$> readMVar gameStateMVar
        updateTextureCache loadedTexturesMVar visualizations loadTx
      ecCheckIfContinue = pure . not . gameExitRequested
      ecGameDebugInfo = \EngineState {..} -> debugPrint <$> readMVar gameStateMVar
  pure $ EngineConfig {..}

lt :: (Ord a, Show a) => Map a Texture -> a -> Texture
lt loadedTextures t =
  fromMaybe (error $ "error loading texture " <> show t <> ", did you forget putting it into all_textures?") $
    mapLookup t loadedTextures

updateTextureCache :: Ord a => MVar (Map a Texture) -> [TexturePlacements a] -> (a -> IO DynamicImage) -> IO [TexturePlacements Texture]
updateTextureCache loadedTexturesMVar visualizations f' =
  modifyMVar loadedTexturesMVar $ \loadedTextures -> do
    let f (acc, loadedTextures') = \case
          TexturePlacements ref area ->
            case mapLookup ref loadedTextures' of
              Nothing -> do
                texture <- either fail pure =<< (runExceptT . loadTexture) =<< f' ref
                pure (TexturePlacements texture area : acc, mapInsert ref texture loadedTextures')
              Just texture -> pure (TexturePlacements texture area : acc, loadedTextures')
          Rectangle t a c -> pure (Rectangle t a c : acc, loadedTextures')
    swap <$> foldlM f ([], loadedTextures) visualizations
