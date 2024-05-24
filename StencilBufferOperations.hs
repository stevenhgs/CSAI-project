module StencilBufferOperations where
import Graphics.UI.GLUT
import Data.Array.Storable
import Foreign (Ptr, nullPtr)
import Foreign.C.Types (CUInt)
import Control.Monad (forM_)


-- Function to read the stencil buffer
readStencilBuffer :: (GLint, GLint) -> (GLsizei, GLsizei) -> IO (StorableArray (GLint, GLint) CUInt)
readStencilBuffer (x, y) (width, height) = do
  arr <- newArray_ ((0, 0), (fromIntegral width - 1, fromIntegral height - 1))
  withStorableArray arr $ \ptr ->
    readPixels (Position x y) (Size (fromIntegral width) (fromIntegral height)) (PixelData StencilIndex UnsignedInt ptr)
  return arr

-- Function to write the stencil buffer
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
  
-- Function to compute the union of two stencil buffers
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