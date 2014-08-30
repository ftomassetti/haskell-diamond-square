{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ghci -fno-ghci-sandbox DiamondSquare.hs

module HeightMap 
where
import Data.Word (Word8)
import System.Random
import Data.Array.Repa as R
import Data.Array.Repa.IO.BMP
import HeightMap.Base (reify,heightMap,heightMap',unitHeightMap,unitHeightMap',getHeight)
import HeightMap.Mesh ()

--------------------------------------------------------------------------------

float2bytes :: Float -> (Word8,Word8,Word8)
float2bytes v = (b,b,b) where b = (floor $ 255.0 * v) :: Word8


main :: IO ()
main = do
  rg    <- getStdGen
  seed1 <- randomRIO (0.0, 1.0) :: IO Float
  seed2 <- randomRIO (0.0, 1.0) :: IO Float
  seed3 <- randomRIO (0.0, 1.0) :: IO Float
  seed4 <- randomRIO (0.0, 1.0) :: IO Float

  let hm = reify $ R.map (\p -> float2bytes $ getHeight p) 
                 $ unitHeightMap rg (320,240) seed1 seed2 seed3 seed4

  --putStrLn $ show hm
  writeImageToBMP "/tmp/HM.bmp" hm
