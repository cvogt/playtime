module SpaceMiner.Textures where

import Data.FileEmbed
import qualified Data.Map as Map
import My.Prelude
import System.IO (FilePath)

data TextureId
  = MainCharacter
  | FloorPlate
  deriving (Eq, Ord, Show)

textureNameMap :: Map TextureId [Char]
textureNameMap =
  Map.fromList
    [ (MainCharacter, "main_character"),
      (FloorPlate, "floor_plate")
    ]

assetsDir :: FilePath
assetsDir = $(makeRelativeToProject "assets" >>= strToExp)
