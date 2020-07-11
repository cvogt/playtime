module SpaceMiner.GL where

import Codec.Picture (DynamicImage (ImageRGBA8), Image (Image), readPng)
import qualified Data.Map as Map (lookup)
import Data.Vector.Storable (unsafeWith)
import GHC.Err (error)
import GHC.Float (double2Float, int2Double, int2Float)
import GHC.Real ((/), fromIntegral)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors as GLU
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.IO
import My.Prelude
import SpaceMiner.Textures
import SpaceMiner.Types

renderGL :: (TextureId -> Texture) -> GLFW.Window -> Dimensions -> [TexturePlacements] -> IO ()
renderGL textures window Dimensions {width, height} texturePlacements = do
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 width height 0 0 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.matrixMode $= GL.Modelview 0
  -- GL.lineSmooth $= GL.Disabled

  -- enable png alpha channel transparancy
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  checkErrorsGLU "before"

  for_ texturePlacements $ \case
    (Rectangle fillType (Pos xd yd) (Dimensions w h) (RGBA r g b a)) -> do
      GL.texture GL.Texture2D $= GL.Disabled
      GL.currentColor $= GL.Color4 (int2Float r / 255) (int2Float g / 255) (int2Float b / 255) (int2Float a / 255)
      mode <- case fillType of
        Solid -> pure GL.Quads
        Border l -> do
          GL.lineWidth $= l
          pure GL.Lines
      GL.renderPrimitive mode $ do
        forM_ [(0, 0), (0, 1), (0, 1), (1, 1), (1, 1), (1, 0), (1, 0), (0, 0)] $ \(x, y) -> do
          GL.vertex $ GL.Vertex2 (double2Float $ xd + (x * w)) (double2Float $ yd + (y * h))
      pure ()
    (TexturePlacements textureId scale (Pos xd yd)) -> do
      let Texture dim texture _ = textures textureId
          Dimensions twidth theight = scale |*| dim
      GL.currentColor $= GL.Color4 @Float 255 255 255 1
      GL.texture GL.Texture2D $= GL.Enabled
      GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
      GL.textureBinding GL.Texture2D $= Just texture
      GL.renderPrimitive GL.Quads
        $ forM_ [(0, 0), (0, 1), (1, 1), (1, 0)]
        $ \(x, y) -> do
          GL.texCoord $ GL.TexCoord2 (double2Float x) (double2Float y) -- remember 1 makes this match the size of the vertex/quad
          GL.vertex $ GL.Vertex2 (double2Float $ (x * twidth) + xd) (double2Float $ (y * theight) + yd)

  checkErrorsGLU "after"
  GLFW.swapBuffers window
  where
    checkErrorsGLU csg = void $ error . ("GLU.errors " <>) . (csg <>) . (": " <>) . show <$> GL.get GLU.errors

loadTextures :: IO (TextureId -> Texture)
loadTextures = do
  m' <- sequence $ loadIntoOpenGL <$> textureNameMap
  pure $ \key -> fromMaybe (error $ "failed to load texture: " <> show key) $ Map.lookup key m'
  where
    loadIntoOpenGL :: FilePath -> IO Texture
    loadIntoOpenGL name = do
      readPng (assetsDir </> name <> ".png") >>= \case
        Right (ImageRGBA8 img@(Image width height dat)) -> unsafeWith dat $ \ptr -> do
          let txSize = GL.TextureSize2D (fromIntegral width) (fromIntegral height)
          [texture] <- GL.genObjectNames 1
          GL.textureBinding GL.Texture2D $= Just texture
          GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 txSize 0 $ GL.PixelData GL.RGBA GL.UnsignedByte ptr
          pure $ Texture (Dimensions (int2Double width) (int2Double height)) texture img
        Left msg -> error $ "loadIntoOpenGL error: " <> msg
        _ -> error "loadIntoOpenGL error: We currently only support png graphic files JuicyPixles reads as ImageRGBA8."
