module Playtime.Textures where

import Data.Aeson (FromJSON, ToJSON)
import Data.FileEmbed
import qualified Data.Map as Map
import qualified Data.Set as Set
import My.Prelude
import System.IO (FilePath)

data TextureId
  = FloorPlate
  | Inventory
  | MainCharacter
  | RedResource
  | TopWall
  | Plane
  deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

textureNameMap :: Map TextureId [Char]
textureNameMap =
  Map.fromList
    [ (FloorPlate, "floor_plate"),
      (Inventory, "inventory"),
      (MainCharacter, "main_character"),
      (RedResource, "red_resource"),
      (TopWall, "top_wall"),
      (Plane, "Plane") -- https://www.cleanpng.com/png-alien-shooter-shooter-game-sprite-pac-man-shoot-em-1502814/
    ]

passableTiles :: Set TextureId
passableTiles = Set.fromList [FloorPlate]

assetsDir :: FilePath
assetsDir = $(makeRelativeToProject "assets" >>= strToExp)

-- convert svg to png
-- https://hackage.haskell.org/package/rasterific-svg-0.3.3.2/docs/Graphics-Rasterific-Svg.html
