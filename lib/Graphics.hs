module Graphics where

import Data.FileEmbed
import GLFWHelpers
import Game
import Graphics.Rendering.OpenGL.GL (Color4 (Color4))
import qualified Data.Map as Map
import My.IO
import My.Prelude
import GHC.Err (error)

assetsDir :: FilePath
assetsDir = $(makeRelativeToProject "assets" >>= strToExp)

data TextureId
  = MainCharacter
  | FloorPlate
  deriving (Eq, Ord, Show)

white :: Color
white = Color4 1.0 1.0 1.0 1.0

loadTextures :: IO (TextureId -> Texture)
loadTextures = do
  let
    m = Map.fromList
      [ (MainCharacter,  "main_character")
      , (FloorPlate,  "floor_plate")
      ]
  m' <- sequence $ m <&> loadIntoOpenGL . (assetsDir </>) . (<>".png")
  pure $ \key -> fromMaybe (error $"failed to load texture: "<> show key) $ Map.lookup key m'

vizualizeGame :: (TextureId -> Texture) -> GameState -> Picture
vizualizeGame textures gameState =
  Pictures $
    [ TexturePlacements (textures FloorPlate) 1 1 $ (uncurry TexturePlacement . unCursorPos <$> (toList $ gsBoard gameState)),
      TexturePlacements (textures MainCharacter) 1 1 $ [uncurry TexturePlacement . unCursorPos $ gsMainCharacterPosition gameState],
      TexturePlacements (textures MainCharacter) 4 4 $ [TexturePlacement 0 0],
      TexturePlacements (textures MainCharacter) 4 4 $ [TexturePlacement 100 100]
    ]
