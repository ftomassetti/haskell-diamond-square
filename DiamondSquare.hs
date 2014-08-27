{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

-- ghci -fno-ghci-sandbox DiamondSquare.hs

module DiamondSquare
where

import Control.Monad.ST
--import qualified Data.Vector as V
--import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

import Data.Word (Word8)
import Data.Array.Repa hiding (map, (++))
import qualified Data.Array.Repa as Repa
import Data.STRef
import System.Random
import Data.Array.Repa.IO.BMP
--import Graphics.Gloss.Raster.Array


newtype Width  = Width Int deriving (Eq,Show,Ord)
newtype Height = Height Int deriving (Eq,Show,Ord)

--------------------------------------------------------------------------------

heightField ::
  (M.Unbox a, RandomGen g) 
    => g 
    -> (Width, Height) 
    -> (Float -> a)
    -> Array U DIM2 a
heightField !rg !dims@(Width w, Height h) !f = do
  let points = computeFieldPoints rg4 dims seed1 seed2 seed3 seed4
  Repa.fromUnboxed shape $ V.map f points
  where 
    (seed1,rg1) = randomR (0.0 :: Float, 1.0 :: Float) rg
    (seed2,rg2) = randomR (0.0 :: Float, 1.0 :: Float) rg1
    (seed3,rg3) = randomR (0.0 :: Float, 1.0 :: Float) rg2
    (seed4,rg4) = randomR (0.0 :: Float, 1.0 :: Float) rg3
    shape       = (Z :. w :. h)


computeFieldPoints ::
  (RandomGen g) 
    => g
    -> (Width, Height)
    -> Float
    -> Float
    -> Float
    -> Float
    -> V.Vector Float
computeFieldPoints !rg !dims@(Width w, Height h) !seed1 !seed2 !seed3 !seed4 = 
  runST $ do 
    rgRef <- newSTRef rg
    v  <- M.new (w * h)
    v' <- diamondStep rgRef dims 0 (nw,seed1) (ne,seed2) (sw,seed3) (se,seed4) v
    V.unsafeFreeze v'
  where
    nw = (0,0)
    ne = (w-1,0)
    sw = (0,h-1)
    se = (w-1,h-1)


diamondStep ::
  (RandomGen g)
    => STRef s g
    -> (Width, Height)
    -> Int
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> M.MVector s Float
    -> ST s (M.MVector s Float)
diamondStep !rgRef !dims !depth !((nwX,nwY),nwV) !((neX,neY),neV) !((swX,swY),swV) !((seX,seY),seV) !v = do
   mV' <- (jitterValue rgRef depth mV)
   M.write v nwIx nwV
   M.write v neIx neV
   M.write v swIx swV  
   M.write v seIx seV
   M.write v mIx mV
   squareStep rgRef dims (depth + 1) ((nwX,nwY),nwV) ((neX,neY),neV) ((swX,swY),swV) ((seX,seY),seV) (m,mV') v >>= return
  where
    nwIx = index2 dims (nwX,nwY)
    neIx = index2 dims (neX,neY)
    swIx = index2 dims (swX,swY)
    seIx = index2 dims (seX,seY)
    m    = (iavg [nwX,neX,swX,seX],iavg [nwY,neY,swY,seY])
    mIx  = index2 dims m
    mV   = avg [nwV,neV,swV,seV]


squareStep ::
  (RandomGen g)
    => STRef s g
    -> (Width, Height)
    -> Int
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> M.MVector s Float
    -> ST s (M.MVector s Float)
squareStep !rgRef !dims !depth !((nwX,nwY),nwV) !((neX,neY),neV) !((swX,swY),swV) !((seX,seY),seV) !((mX,mY),mV) !v
  | done      = return v
  | otherwise = do
      diamondStep rgRef dims depth' ((nwX,nwY),nwV) ((nX,nY),nV) ((wX,wY),wV) ((mX,mY),mV) v >>=
        diamondStep rgRef dims depth' ((nX,nY),nV) ((neX,neY),neV) ((mX,mY),mV) ((eX,eY),eV) >>=
        diamondStep rgRef dims depth' ((wX,wY),wV) ((mX,mY),mV) ((swX,swY),swV) ((sX,sY),sV) >>=
        diamondStep rgRef dims depth' ((mX,mY),mV) ((eX,eY),eV) ((sX,sY),sV) ((seX,seY),seV) >>=
        return
  where
    depth'     = depth + 1
    done       = (abs $ nwX - seX) * (abs $ nwY - seY) <= 1
    (nX,nY,nV) = (iavg [nwX,neX],iavg [nwY,neY],avg [nwV,neV])
    (eX,eY,eV) = (iavg [neX,seX],iavg [neY,seY],avg [neV,seV])
    (wX,wY,wV) = (iavg [nwX,swX],iavg [nwY,swY],avg [nwV,swV])
    (sX,sY,sV) = (iavg [swX,seX],iavg [swY,seY],avg [swV,seV])


jitterValue ::
  (RandomGen g) 
    => STRef s g 
    -> Int 
    -> Float
    -> ST s Float
jitterValue rgRef depth value = do
  rg <- readSTRef rgRef
  let (amount, rg') = randomR (-1.0 :: Float,1.0 :: Float) rg
  let depth'        = (fromIntegral depth)
  let power         = 1.0 / depth'
  writeSTRef rgRef rg'
  return $ clamp 0.0 1.0 $ value + (amount * power)

--------------------------------------------------------------------------------

clamp :: Ord a => a -> a -> a -> a
clamp lo hi n = min (max n lo) hi


avg :: Fractional a => [a] -> a
avg xs = sum xs / (fromIntegral $ length xs)


iavg :: [Int] -> Int
iavg xs = floor $ sum (map fromIntegral xs) / (fromIntegral $ length xs)


index2 :: (Width,Height) -> (Int,Int) -> Int
index2 (Width w,_) (i,j) = (i * w) + j


index1 :: (Width,Height) -> Int -> (Int,Int)
index1 (Width w,_) i = (i `div` w,i `mod` w)


floatToBytes :: Float -> (Word8,Word8,Word8)
floatToBytes v = (b,b,b) where b = (floor $ 255.0 * v) :: Word8


main :: IO ()
main = do
  rg <- getStdGen
  let (w,h) = (128,128)
  let dims  = (Width w,Height h)

  let hf = heightField rg dims floatToBytes

  writeImageToBMP "/tmp/heightfield.bmp" hf
