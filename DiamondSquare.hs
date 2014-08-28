{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

-- ghci -fno-ghci-sandbox DiamondSquare.hs

module DiamondSquare (HeightMap(..), heightField)
where

--import Control.Monad
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

--------------------------------------------------------------------------------

type Point     = (Int,Int)
newtype Width  = Width Int deriving (Eq,Show,Ord)
newtype Height = Height Int deriving (Eq,Show,Ord)

data HeightMap a = HeightMap { getValues :: !(Array U DIM2 a) }
     deriving (Show,Eq)

--------------------------------------------------------------------------------

class (M.Unbox a, Ord a, Random a) => SeedValue a where

  avg :: [a] -> a

  jitter :: RandomGen g => g -> Int -> a -> (g,a)


instance SeedValue Int where

  avg xs = sum xs `div` length xs

  jitter rg _ value = (rg', max 0 $ value + amount)
    where
      (amount, rg') = randomR (-1,1) rg


instance SeedValue Float where

  avg xs = sum xs / (fromIntegral $ length xs)

  jitter rg depth value = (rg', clamp 0.0 1.0 $ value + (amount * power))
    where
      (amount, rg') = randomR (-1.0,1.0) rg
      power         = 1.0 / (fromIntegral depth)

--------------------------------------------------------------------------------

heightField ::
  (M.Unbox a, RandomGen g, SeedValue b) 
    => g               -- ^ Random seed value generator
    -> (Width, Height) -- ^ (Width,Height) of the height map in units
    -> b               -- ^ NW seed value
    -> b               -- ^ NE seed value
    -> b               -- ^ SW seed value
    -> b               -- ^ SE seed value
    -> (b -> a)        -- ^
    -> HeightMap a
heightField !rg !dims@(Width w, Height h) !seed1 !seed2 !seed3 !seed4 !f = do
  let points  = computeFieldPoints rg dims seed1 seed2 seed3 seed4
  let points' = Repa.fromUnboxed (Z :. w :. h) $ V.map f points
  HeightMap $ if w == h 
              then points' 
              else Repa.computeUnboxedS $ Repa.transpose points'


computeFieldPoints ::
  (RandomGen g, SeedValue b) 
    => g               -- ^ Random seed value generator
    -> (Width, Height) -- ^ (Width,Height) of the height map in units
    -> b               -- ^ NW seed value
    -> b               -- ^ NE seed value
    -> b               -- ^ SW seed value
    -> b               -- ^ SE seed value
    -> V.Vector b
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
  (RandomGen g, SeedValue b)
    => STRef s g
    -> (Width, Height) -- ^ (Width,Height) of the height map in units
    -> Int             -- ^ Current recursion depth in the algorithm
    -> (Point, b)      -- ^ NW ((x,y) coordinate, seed-value)
    -> (Point, b)      -- ^ NE ((x,y) coordinate, seed-value)
    -> (Point, b)      -- ^ SW ((x,y) coordinate, seed-value)
    -> (Point, b)      -- ^ SE ((x,y) coordinate, seed-value)
    -> M.MVector s b   -- ^ The result vector
    -> ST s (M.MVector s b)
diamondStep !rgRef !dims !depth !((nwX,nwY),nwV) !((neX,neY),neV) !((swX,swY),swV) !((seX,seY),seV) !v = do
   mV' <- jitterValue mV
   M.unsafeWrite v nwIx nwV
   M.unsafeWrite v neIx neV
   M.unsafeWrite v swIx swV  
   M.unsafeWrite v seIx seV
   M.unsafeWrite v mIx mV
   squareStep rgRef 
              dims 
              (depth + 1) 
              ((nwX,nwY),nwV) 
              ((neX,neY),neV) 
              ((swX,swY),swV) 
              ((seX,seY),seV) 
              (m,mV') 
              v >>= return
  where

    jitterValue value = do
      rg <- readSTRef rgRef
      let (rg',delta) = jitter rg depth value
      writeSTRef rgRef rg'
      return delta

    nwIx = index2 dims (nwX,nwY)
    neIx = index2 dims (neX,neY)
    swIx = index2 dims (swX,swY)
    seIx = index2 dims (seX,seY)
    m    = (avg [nwX,neX,swX,seX],avg [nwY,neY,swY,seY])
    mIx  = index2 dims m
    mV   = avg [nwV,neV,swV,seV]


squareStep ::
  (RandomGen g, SeedValue b)
    => STRef s g
    -> (Width, Height) -- ^ (Width,Height) of the height map in units
    -> Int             -- ^ Current recursion depth in the algorithm
    -> (Point, b)      -- ^ NW ((x,y) coordinate, seed-value)
    -> (Point, b)      -- ^ NE ((x,y) coordinate, seed-value)
    -> (Point, b)      -- ^ SW ((x,y) coordinate, seed-value)
    -> (Point, b)      -- ^ SE ((x,y) coordinate, seed-value)
    -> (Point, b)      -- ^ Midpoint ((x,y) coordinate, seed-value)
    -> M.MVector s b   -- ^ The result vector
    -> ST s (M.MVector s b)
squareStep !rgRef !dims !depth !((nwX,nwY),nwV) !((neX,neY),neV) !((swX,swY),swV) !((seX,seY),seV) !((mX,mY),mV) !v
  | done      = return v
  | otherwise = do
      diamondStep rgRef 
                  dims 
                  depth' 
                  ((nwX,nwY),nwV) 
                  ((nX,nY),nV) 
                  ((wX,wY),wV) 
                  ((mX,mY),mV) v >>=
        diamondStep rgRef 
                    dims 
                    depth' 
                    ((nX,nY),nV) 
                    ((neX,neY),neV) 
                    ((mX,mY),mV) 
                    ((eX,eY),eV) >>=
        diamondStep rgRef 
                    dims 
                    depth'
                    ((wX,wY),wV) 
                    ((mX,mY),mV) 
                    ((swX,swY),swV) 
                    ((sX,sY),sV) >>=
        diamondStep rgRef 
                    dims 
                    depth' 
                    ((mX,mY),mV) 
                    ((eX,eY),eV) 
                    ((sX,sY),sV) 
                    ((seX,seY),seV) >>=
        return
  where
    depth'     = depth + 1
    done       = (abs $ nwX - seX) <= 1 && (abs $ nwY - seY) <= 1
    (nX,nY,nV) = (avg [nwX,neX],avg [nwY,neY],avg [nwV,neV])
    (eX,eY,eV) = (avg [neX,seX],avg [neY,seY],avg [neV,seV])
    (wX,wY,wV) = (avg [nwX,swX],avg [nwY,swY],avg [nwV,swV])
    (sX,sY,sV) = (avg [swX,seX],avg [swY,seY],avg [swV,seV])

--------------------------------------------------------------------------------


index2 :: (Width, Height) -> (Int, Int) -> Int
index2 (Width w,Height h) (i,j) = (i * h) + j


clamp :: Ord a => a -> a -> a -> a
clamp lo hi n = min (max n lo) hi


floatToBytes :: Float -> (Word8,Word8,Word8)
floatToBytes v = (b,b,b) where b = (floor $ 255.0 * v) :: Word8

--------------------------------------------------------------------------------

main :: IO ()
main = do
  rg    <- getStdGen
  seed1 <- randomRIO (0.0, 1.0)
  seed2 <- randomRIO (0.0, 1.0)
  seed3 <- randomRIO (0.0, 1.0)
  seed4 <- randomRIO (0.0, 1.0)
  let (w,h) = (300,200)
  let dims  = (Width w,Height h)

  let hf = heightField rg dims seed1 seed2 seed3 seed4 floatToBytes
  writeImageToBMP "/tmp/heightfield.bmp" $ getValues hf
