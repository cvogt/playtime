module SpaceMiner.Textures where

import Data.Aeson (FromJSON, ToJSON)
import Data.FileEmbed
import qualified Data.Map as Map
import qualified Data.Set as Set
import My.Prelude
import System.IO (FilePath)

data TextureId
  = MainCharacter
  | FloorPlate
  | TopWall
  deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

textureNameMap :: Map TextureId [Char]
textureNameMap =
  Map.fromList
    [ (FloorPlate, "floor_plate"),
      (MainCharacter, "main_character"),
      (TopWall, "top_wall")
    ]

passableTiles :: Set TextureId
passableTiles = Set.fromList [FloorPlate]

assetsDir :: FilePath
assetsDir = $(makeRelativeToProject "assets" >>= strToExp)
