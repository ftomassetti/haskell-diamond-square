{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module HeightMap.Base (HeightMap, HeightMap'
  ,Point(Point), getX, getY, getHeight
  , reify
  , heightMap, heightMap', unitHeightMap, unitHeightMap'
  , float2bytes)
where
import Control.Monad.Identity
import Control.Monad.ST
import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable hiding (length)
import qualified Data.Vector.Unboxed as U hiding (length,sum)
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Array.Repa hiding (map, (++))
import qualified Data.Array.Repa as R
import System.Random
import Data.Word (Word8)

-- Misc

float2bytes :: Float -> (Word8,Word8,Word8)
float2bytes v = (b,b,b) where b = (floor $ 255.0 * v) :: Word8

--------------------------------------------------------------------------------

-- | Immediate, 2-dimensional heightmap array
type HeightMap a  = Array U DIM2 a

-- | Delayed, 2-dimensional heightmap array
type HeightMap' a = Array D DIM2 a


newtype Width = Width Int 
  deriving (Eq,Show,Ord)


newtype Height = Height Int 
  deriving (Eq,Show,Ord)


newtype Point a = Point (Float,Float,a) 
  deriving (Eq,Vector U.Vector,MVector U.MVector,U.Unbox)


getX :: Point a -> Float
getX (Point (x,_,_)) = x


getY :: Point a -> Float
getY (Point (_,y,_)) = y


getHeight :: Point a -> a
getHeight (Point (_,_,h)) = h


instance Show a => Show (Point a) where

  show (Point (x,y,h)) = 
    "<(" ++ (show x) ++ "," ++ (show y) ++ ") " ++ (show h) ++ ">"

--------------------------------------------------------------------------------

class (U.Unbox a, Ord a, Random a) => SeedValue a where

  empty :: a

  avg :: [a] -> a

  jitter :: RandomGen g => g -> Int -> a -> (g,a)

--------------------------------------------------------------------------------

-- |
reify :: (UM.Unbox e,Shape sh) => Array D sh e -> Array U sh e
reify arr = runIdentity . R.computeP $ arr


-- |
heightMap ::
  (RandomGen g, SeedValue a) 
    => g         -- ^ Random seed value generator
    -> (Int,Int) -- ^ (Width,Height) of the height map in units
    -> a         -- ^ NW seed value
    -> a         -- ^ NE seed value
    -> a         -- ^ SW seed value
    -> a         -- ^ SE seed value
    -> HeightMap a
heightMap !rg !(w,h) !seed1 !seed2 !seed3 !seed4 = 
  reify $ heightMap' rg (w,h) seed1 seed2 seed3 seed4


-- |
heightMap' ::
  (RandomGen g, SeedValue a) 
    => g         -- ^ Random seed value generator
    -> (Int,Int) -- ^ (Width,Height) of the height map in units
    -> a         -- ^ NW seed value
    -> a         -- ^ NE seed value
    -> a         -- ^ SW seed value
    -> a         -- ^ SE seed value
    -> HeightMap' a
heightMap' !rg !(w,h) !seed1 !seed2 !seed3 !seed4 =
  R.delay . R.fromUnboxed (Z :. w' :. h') $ 
               computeFieldPoints
                  rg (Width w', Height h') seed1 seed2 seed3 seed4
  where
    (w',h') = (h,w)


-- |
unitHeightMap ::
  (RandomGen g, SeedValue a)
    => g         -- ^ Random seed value generator
    -> (Int,Int) -- ^ (Width,Height) of the height map in units
    -> a         -- ^ NW seed value
    -> a         -- ^ NE seed value
    -> a         -- ^ SW seed value
    -> a         -- ^ SE seed value
    -> HeightMap (Point a)
unitHeightMap !rg !(w,h) !seed1 !seed2 !seed3 !seed4 =
  reify $ unitHeightMap' rg (w,h) seed1 seed2 seed3 seed4


-- |
unitHeightMap' ::
  (RandomGen g, SeedValue a)
    => g         -- ^ Random seed value generator
    -> (Int,Int) -- ^ (Width,Height) of the height map in units
    -> a         -- ^ NW seed value
    -> a         -- ^ NE seed value
    -> a         -- ^ SW seed value
    -> a         -- ^ SE seed value
    -> HeightMap' (Point a)
unitHeightMap' !rg !(w,h) !seed1 !seed2 !seed3 !seed4 = 
  R.traverse hm id update
  where
    hm = heightMap' rg (w,h) seed1 seed2 seed3 seed4
    w' = fromIntegral $ w - 1
    h' = fromIntegral $ h - 1
    update lookup index@(Z :. i :. j) = 
      Point (fromIntegral i / w',fromIntegral j / h',lookup index)

--------------------------------------------------------------------------------

-- |
computeFieldPoints ::
  (RandomGen g, SeedValue a) 
    => g               -- ^ Random seed value generator
    -> (Width, Height) -- ^ (Width,Height) of the height map in units
    -> a               -- ^ NW seed value
    -> a               -- ^ NE seed value
    -> a               -- ^ SW seed value
    -> a               -- ^ SE seed value
    -> U.Vector a
computeFieldPoints !rg !dims@(Width w, Height h) !seed1 !seed2 !seed3 !seed4 = 
  runST $ do 
    v   <- UM.new $ (w * h)
    UM.set v empty
    v'  <- diamond rg dims 0 (nw,seed1) (ne,seed2) (sw,seed3) (se,seed4) v
    U.unsafeFreeze v'
  where
    nw = (0,0)
    ne = (w,0)
    sw = (0,h)
    se = (w,h)


-- |
diamond ::
  (RandomGen g, SeedValue a)
    => g
    -> (Width, Height) -- ^ (Width,Height) of the height map in units
    -> Int             -- ^ Current recursion depth in the algorithm
    -> ((Int,Int), a)  -- ^ NW ((x,y) coordinate, seed-value)
    -> ((Int,Int), a)  -- ^ NE ((x,y) coordinate, seed-value)
    -> ((Int,Int), a)  -- ^ SW ((x,y) coordinate, seed-value)
    -> ((Int,Int), a)  -- ^ SE ((x,y) coordinate, seed-value)
    -> UM.MVector s a   -- ^ The result vector
    -> ST s (UM.MVector s a)
diamond !rg
        !dims 
        !depth 
        !((nwX,nwY),nwV) 
        !((neX,neY),neV) 
        !((swX,swY),swV) 
        !((seX,seY),seV) 
        !v
  | done (nwX,nwY) (neX,neY) (swX,swY) (seX,seY) m = do
    unsafeWrite v mIx mV
    return v
  | otherwise = do
      square rg'
             dims 
             (depth + 1) 
             ((nwX,nwY),nwV) 
             ((neX,neY),neV) 
             ((swX,swY),swV) 
             ((seX,seY),seV) 
             (m,mV') 
              v >>= return
  where
    index2 (_,Height h) (i,j) = (i * h) + j
    (rg',mV') = jitter rg depth mV
    m    = (avg [nwX,neX,swX,seX],avg [nwY,neY,swY,seY])
    mIx  = index2 dims m
    mV   = avg [nwV,neV,swV,seV]


-- |
square ::
  (RandomGen g, SeedValue a)
    => g
    -> (Width, Height) -- ^ (Width,Height) of the height map in units
    -> Int             -- ^ Current recursion depth in the algorithm
    -> ((Int,Int), a)  -- ^ NW ((x,y) coordinate, seed-value)
    -> ((Int,Int), a)  -- ^ NE ((x,y) coordinate, seed-value)
    -> ((Int,Int), a)  -- ^ SW ((x,y) coordinate, seed-value)
    -> ((Int,Int), a)  -- ^ SE ((x,y) coordinate, seed-value)
    -> ((Int,Int), a)  -- ^ Midpoint ((x,y) coordinate, seed-value)
    -> UM.MVector s a   -- ^ The result vector
    -> ST s (UM.MVector s a)
square !rg
       !dims 
       !depth 
       !((nwX,nwY),nwV) 
       !((neX,neY),neV) 
       !((swX,swY),swV) 
       !((seX,seY),seV) 
       !((mX,mY),mV) 
       !v =
  do
    diamond rg1 dims depth' ((nwX,nwY),nwV) ((nX,nY),nV) ((wX,wY),wV) ((mX,mY),mV) v >>=
     diamond rg2 dims depth' ((nX,nY),nV) ((neX,neY),neV) ((mX,mY),mV) ((eX,eY),eV) >>=
     diamond rg3 dims depth' ((wX,wY),wV) ((mX,mY),mV) ((swX,swY),swV) ((sX,sY),sV) >>=
     diamond rg4 dims depth' ((mX,mY),mV) ((eX,eY),eV) ((sX,sY),sV) ((seX,seY),seV) >>=
     return
  where
    (rg1,rg2)  = split rg
    (rg3,rg4)  = split rg1
    depth'     = depth + 1
    (nX,nY,nV) = (avg [nwX,neX],avg [nwY,neY],avg [nwV,neV])
    (eX,eY,eV) = (avg [neX,seX],avg [neY,seY],avg [neV,seV])
    (wX,wY,wV) = (avg [nwX,swX],avg [nwY,swY],avg [nwV,swV])
    (sX,sY,sV) = (avg [swX,seX],avg [swY,seY],avg [swV,seV])


-- | Done condition test
done :: (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
done (nwX,nwY) (neX,neY) (swX,swY) (seX,seY) (mX,mY) = l * w <= 1
  where
    l = abs $ nwY - swY
    w = abs $ nwX - neX
{-# INLINE done #-}


--done :: (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
--done (nwX,nwY) (neX,neY) (swX,swY) (seX,seY) (mX,mY)
--  | (nwX,nwY) == (mX,mY) = True
--  | (neX,neY) == (mX,mY) = True
--  | (swX,swY) == (mX,mY) = True
--  | (seX,seY) == (mX,mY) = True
--  | otherwise            = False
--{-# INLINE done #-}


-- | Clamp a value toa given range
clamp :: Ord a => (a,a) -> a -> a
clamp (lo,hi) n = min (max n lo) hi
{-# INLINE clamp #-}

--------------------------------------------------------------------------------

instance SeedValue Int where

  empty  = 0

  avg xs = sum xs `div` length xs

  jitter rg _ value = (rg', max 0 $ value + amount)
    where
      (amount, rg') = randomR (-1,1) rg


instance SeedValue Float where

  empty  = 0.0

  avg xs = sum xs / (fromIntegral $ length xs)

  jitter rg depth value = (rg', clamp (0.0,1.0) $ value + (amount * power))
    where
      (amount, rg') = randomR (-1.0,1.0) rg
      power         = 1.0 / (fromIntegral depth)

