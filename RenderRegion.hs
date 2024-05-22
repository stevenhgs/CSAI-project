module RenderRegion where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Region

import Data.Array.Storable
import Foreign (Ptr, nullPtr)
import Foreign.C.Types (CUInt)
import Control.Monad (forM_, when)


drawSphere :: GLdouble -> DisplayCallback
drawSphere radius = do
  color $ Color3 1 0 (0 :: GLfloat)
  renderObject Solid (Sphere' radius 40 40)

drawCube :: GLdouble -> DisplayCallback
drawCube side = do
  color $ Color3 1 0 (0 :: GLfloat)
  renderObject Solid (Cube side)

drawTranslate :: DisplayCallback -> PointGS -> DisplayCallback
drawTranslate cb (x, y, z) = do
  preservingMatrix $ do
    translate $ Vector3 x y z
    cb

drawOutside :: DisplayCallback -> DisplayCallback
drawOutside dc = do
  stencilTest $= Enabled
  colorMask $= Color4 Disabled Disabled Disabled Disabled
  stencilOp $= (OpReplace, OpReplace, OpReplace)
  stencilFunc $= (Always, 1, 0xFF)
  stencilMask $= 0xFF

  dc

  colorMask $= Color4 Enabled Enabled Enabled Enabled
  stencilFunc $= (Notequal, 1, 0xFF)
  stencilMask $= 0x00
  color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
  renderPrimitive Quads $ do
    vertex (Vertex2 (-1.0) (-1.0) :: Vertex2 GLfloat)
    vertex (Vertex2 1.0 (-1.0) :: Vertex2 GLfloat)
    vertex (Vertex2 1.0 1.0 :: Vertex2 GLfloat)
    vertex (Vertex2 (-1.0) 1.0 :: Vertex2 GLfloat)

  -- Disable stencil testing
  stencilTest $= Disabled





-- Helper function to read the stencil buffer
readStencilBuffer :: (GLint, GLint) -> (GLsizei, GLsizei) -> IO (StorableArray (GLint, GLint) CUInt)
readStencilBuffer (x, y) (width, height) = do
  arr <- newArray_ ((0, 0), (fromIntegral width - 1, fromIntegral height - 1))
  withStorableArray arr $ \ptr ->
    readPixels (Position x y) (Size (fromIntegral width) (fromIntegral height)) (PixelData StencilIndex UnsignedInt ptr)
  return arr

-- Helper function to write the stencil buffer
writeStencilBuffer :: (GLint, GLint) -> (GLsizei, GLsizei) -> StorableArray (GLint, GLint) CUInt -> IO ()
writeStencilBuffer (x, y) (width, height) arr =
  withStorableArray arr $ \ptr ->
    drawPixels (Size (fromIntegral width) (fromIntegral height)) (PixelData StencilIndex UnsignedInt ptr)

-- Function to compute the intersection of two stencil buffers
computeIntersection :: (GLsizei, GLsizei) -> StorableArray (GLint, GLint) CUInt -> StorableArray (GLint, GLint) CUInt -> IO (StorableArray (GLint, GLint) CUInt)
computeIntersection (width, height) buf1 buf2 = do
  intersectionBuffer <- newArray_ ((0, 0), (fromIntegral width - 1, fromIntegral height - 1))
  forM_ [0..(fromIntegral width - 1)] $ \i ->
    forM_ [0..(fromIntegral height - 1)] $ \j -> do
      val1 <- readArray buf1 (i, j)
      val2 <- readArray buf2 (i, j)
      let interVal = if val1 == 1 && val2 == 1 then 1 else 0
      writeArray intersectionBuffer (i, j) interVal
  return intersectionBuffer
  
-- Function to compute the intersection of two stencil buffers
computeUnion :: (GLsizei, GLsizei) -> StorableArray (GLint, GLint) CUInt -> StorableArray (GLint, GLint) CUInt -> IO (StorableArray (GLint, GLint) CUInt)
computeUnion (width, height) buf1 buf2 = do
  intersectionBuffer <- newArray_ ((0, 0), (fromIntegral width - 1, fromIntegral height - 1))
  forM_ [0..(fromIntegral width - 1)] $ \i ->
    forM_ [0..(fromIntegral height - 1)] $ \j -> do
      val1 <- readArray buf1 (i, j)
      val2 <- readArray buf2 (i, j)
      let interVal = if val1 == 1 || val2 == 1 then 1 else 0
      writeArray intersectionBuffer (i, j) interVal
  return intersectionBuffer


-- Main function to draw the intersection
drawIntersection :: DisplayCallback -> DisplayCallback -> DisplayCallback
drawIntersection dc1 dc2 = do
  (Position x y, Size width height) <- get viewport

  -- Get the previous state and save it
  prevStencilTestStatus <- get stencilTest
  prevColorMask <- get colorMask
  prevStencilFunc <- get stencilFunc
  prevStencilOp <- get stencilOp
  prevStencilBuffer <- readStencilBuffer (x, y) (width, height)

  -- Check if prevColorMask is Disabled
  let isColorMaskDisabled = case prevColorMask of
        Color4 Disabled Disabled Disabled Disabled -> True
        _ -> False

  stencilTest $= Enabled
  colorMask $= Color4 Disabled Disabled Disabled Disabled

  -- Get first mask
  clear [StencilBuffer]
  stencilFunc $= (Always, 1, 0xFF)
  stencilOp $= (OpReplace, OpReplace, OpReplace)
  dc1
  stencilBuffer1 <- readStencilBuffer (x, y) (width, height)

  -- Get second mask
  clear [StencilBuffer]
  stencilFunc $= (Always, 1, 0xFF)
  stencilOp $= (OpReplace, OpReplace, OpReplace)
  dc2
  stencilBuffer2 <- readStencilBuffer (x, y) (width, height)

  -- Compute the intersection of the two stencil buffers
  intersectionStencilBuffer <- computeIntersection (width, height) stencilBuffer1 stencilBuffer2
  fullIntersectionForDrawingStencilBuffer <- computeIntersection (width, height) prevStencilBuffer intersectionStencilBuffer

  clear [StencilBuffer]
  writeStencilBuffer (x, y) (width, height) intersectionStencilBuffer
  -- Restore the color mask
  colorMask $= prevColorMask

  -- Now you know something really has to be drawn
  when (not isColorMaskDisabled) $ do
    -- Restore the intersection result to the stencil buffer
    clear [StencilBuffer]
    writeStencilBuffer (x, y) (width, height) fullIntersectionForDrawingStencilBuffer
    -- Final rendering based on the intersection result
    stencilFunc $= (Equal, 1, 0xFF)
    stencilOp $= (OpKeep, OpKeep, OpKeep)
    -- Draw your final scene based on the intersection result
    putStrLn "Drawing"
    dc2

    -- fill StencilBuffer with all ones
    clear [StencilBuffer]
    writeStencilBuffer (x, y) (width, height) prevStencilBuffer
    colorMask $= prevColorMask

  -- Restore the stencil test state
  stencilTest $= prevStencilTestStatus
  stencilFunc $= prevStencilFunc
  stencilOp $= prevStencilOp
  
  


drawUnion :: DisplayCallback -> DisplayCallback -> DisplayCallback
drawUnion dc1 dc2 = do
    prevColorMask <- get colorMask
    -- Check if prevColorMask is Disabled
    let isColorMaskDisabled = case prevColorMask of
          Color4 Disabled Disabled Disabled Disabled -> True
          _ -> False
    -- if color mask is disabled we need to get the stencil buffer
    when (isColorMaskDisabled) $ do
      (Position x y, Size width height) <- get viewport
      -- Get the previous state and save it
      prevStencilTestStatus <- get stencilTest
      prevStencilFunc <- get stencilFunc
      prevStencilOp <- get stencilOp
      prevStencilBuffer <- readStencilBuffer (x, y) (width, height)
      -- set 
      stencilTest $= Enabled
      colorMask $= Color4 Disabled Disabled Disabled Disabled
      -- Get first mask
      clear [StencilBuffer]
      stencilFunc $= (Always, 1, 0xFF)
      stencilOp $= (OpReplace, OpReplace, OpReplace)
      dc1
      stencilBuffer1 <- readStencilBuffer (x, y) (width, height)
      -- Get second mask
      clear [StencilBuffer]
      stencilFunc $= (Always, 1, 0xFF)
      stencilOp $= (OpReplace, OpReplace, OpReplace)
      dc2
      stencilBuffer2 <- readStencilBuffer (x, y) (width, height)
      -- Compute the union of the two stencil buffers
      unionStencilBuffer <- computeUnion (width, height) stencilBuffer1 stencilBuffer2
      -- write the unionStencilBuffer to the StencilBuffer
      clear [StencilBuffer]
      writeStencilBuffer (x, y) (width, height) unionStencilBuffer
      -- Restore the stencil test state
      colorMask $= prevColorMask
      stencilTest $= prevStencilTestStatus
      stencilFunc $= prevStencilFunc
      stencilOp $= prevStencilOp
    
    -- if color mask is not disabled we need to draw
    when (not isColorMaskDisabled) $ do
      dc1
      dc2

-- FOR TESTING
display :: DisplayCallback
display = do
  clear [ ColorBuffer, DepthBuffer, StencilBuffer]
  drawIntersection (drawTranslate (drawCube 0.2) (0.1, 0, 0)) (drawCube 0.2)
  flush

main :: IO()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, WithStencilBuffer]
  _window <- createWindow "GeoServer"
  clearColor $= Color4 0.0 0.0 0.0 1.0
  displayCallback $= display
  mainLoop
