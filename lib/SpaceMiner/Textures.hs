module SpaceMiner.Textures where

import Data.Aeson (FromJSON, ToJSON)
import Data.FileEmbed
import qualified Data.Map as Map
import qualified Data.Set as Set
import My.Prelude
import System.IO (FilePath)

data TextureId
  = FloorPlate
  | Inventory
  | GermanFlag
  | MainCharacter
  | RedResource
  | TopWall
  deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

textureNameMap :: Map TextureId [Char]
textureNameMap =
  Map.fromList
    [ (FloorPlate, "floor_plate"),
      (Inventory, "inventory"),
      (GermanFlag, "german_flag"),
      (MainCharacter, "main_character"),
      (RedResource, "red_resource"),
      (TopWall, "top_wall")
    ]

passableTiles :: Set TextureId
passableTiles = Set.fromList [FloorPlate]

assetsDir :: FilePath
assetsDir = $(makeRelativeToProject "assets" >>= strToExp)
