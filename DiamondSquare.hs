{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}

-- ghci -fno-ghci-sandbox DiamondSquare.hs

module DiamondSquare
where

import Control.Monad.ST
import Data.Array.ST

--import qualified Data.Vector as V
--import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

import Data.Array.Repa hiding (map, (++))
import qualified Data.Array.Repa as Repa
import Control.Monad.State
import Data.STRef
import System.Random
--import Data.Array.Repa.IO.BMP
--import Graphics.Gloss.Raster.Array


newtype Width  = Width Int deriving (Eq,Show,Ord)
newtype Height = Height Int deriving (Eq,Show,Ord)

--------------------------------------------------------------------------------

computePoints rg dims@(Width w, Height h) nwSeed neSeed swSeed seSeed =
  runSTArray $ do 
      rgRef <- newSTRef rg
      let size = (w * h) - 1
      arr  <- newArray (0,size) 0.0
      arr' <- diamondStep rgRef dims 0 (nw,nwSeed) (ne,neSeed) (sw,swSeed) (se,seSeed) arr
      return arr'
  where
    nw = (0,0)
    ne = (w-1,0)
    sw = (0,h-1)
    se = (w-1,h-1)

diamondStep :: 
  (MArray t Float (ST s), RandomGen g) 
    => STRef s g
    -> (Width, Height)
    -> Int
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> ((Int, Int), Float)
    -> t Int Float
    -> ST s (t Int Float)
diamondStep rgRef dims depth ((nwX,nwY),nwV) ((neX,neY),neV) ((swX,swY),swV) ((seX,seY),seV) arr = do
   rg <- readSTRef rgRef
   let (jitter, rg') = randomR (-1.0 :: Float,1.0 :: Float) rg
   let power         = 1.0 / (fromIntegral $ 2 ^ depth)
   writeSTRef rgRef rg'
   writeArray arr nwIx nwV
   writeArray arr neIx neV
   writeArray arr swIx swV  
   writeArray arr seIx seV
   writeArray arr mIx mV
   squareStep rgRef dims (depth + 1) ((nwX,nwY),nwV) ((neX,neY),neV) ((swX,swY),swV) ((seX,seY),seV) (m,mV + (jitter * power)) arr >>= return
  where
    nwIx = index2 dims (nwX,nwY)
    neIx = index2 dims (neX,neY)
    swIx = index2 dims (swX,swY)
    seIx = index2 dims (seX,seY)
    m    = (iavg [nwX,neX,swX,seX],iavg [nwY,neY,swY,seY])
    mIx  = index2 dims m
    mV   = avg [nwV,neV,swV,seV]


squareStep ::
  (MArray t Float (ST s), RandomGen g) 
  => STRef s g
  -> (Width, Height)
  -> Int
  -> ((Int, Int), Float)
  -> ((Int, Int), Float)
  -> ((Int, Int), Float)
  -> ((Int, Int), Float)
  -> ((Int, Int), Float)
  -> t Int Float
  -> ST s (t Int Float)
squareStep rgRef dims depth ((nwX,nwY),nwV) ((neX,neY),neV) ((swX,swY),swV) ((seX,seY),seV) ((mX,mY),mV) arr
  | done      = return arr
  | otherwise = do
      diamondStep rgRef dims depth' ((nwX,nwY),nwV) ((nX,nY),nV) ((wX,wY),wV) ((mX,mY),mV) arr >>=
        diamondStep rgRef dims depth' ((nX,nY),nV) ((neX,neY),neV) ((mX,mY),mV) ((eX,eY),eV)   >>=
        diamondStep rgRef dims depth' ((wX,wY),wV) ((mX,mY),mV) ((swX,swY),swV) ((sX,sY),sV)   >>=
        diamondStep rgRef dims depth' ((mX,mY),mV) ((eX,eY),eV) ((sX,sY),sV) ((seX,seY),seV)   >>=
        return
  where
    depth'     = depth + 1
    done       = (abs $ nwX - seX) * (abs $ nwY - seY) <= 1
    (nX,nY,nV) = (iavg [nwX,neX],iavg [nwY,neY],avg [nwV,neV])
    (eX,eY,eV) = (iavg [neX,seX],iavg [neY,seY],avg [neV,seV])
    (wX,wY,wV) = (iavg [nwX,swX],iavg [nwY,swY],avg [nwV,swV])
    (sX,sY,sV) = (iavg [swX,seX],iavg [swY,seY],avg [swV,seV])

--------------------------------------------------------------------------------

computePoints' ::
  (RandomGen g) 
  => g
  -> (Width, Height)
  -> Float
  -> Float
  -> Float
  -> Float
  -> V.Vector Float
computePoints' rg dims@(Width w, Height h) nwSeed neSeed swSeed seSeed = 
  runST $ do 
    rgRef <- newSTRef rg
    let size = (w * h)
    v  <- M.new size
    v' <- diamondStep' rgRef dims 0 (nw,nwSeed) (ne,neSeed) (sw,swSeed) (se,seSeed) v
    V.unsafeFreeze v'
  where
    nw = (0,0)
    ne = (w-1,0)
    sw = (0,h-1)
    se = (w-1,h-1)


--diamondStep' ::
--  (M.MVector v Float, RandomGen g) 
--    => STRef s g
--    -> (Width, Height)
--    -> Int
--    -> ((Int, Int), Float)
--    -> ((Int, Int), Float)
--    -> ((Int, Int), Float)
--    -> ((Int, Int), Float)
--    -> v s Float
--    -> ST s (v s Float)
diamondStep' rgRef dims depth ((nwX,nwY),nwV) ((neX,neY),neV) ((swX,swY),swV) ((seX,seY),seV) v = do
   rg <- readSTRef rgRef
   let (jitter, rg') = randomR (-1.0 :: Float,1.0 :: Float) rg
   let power         = 1.0 / (fromIntegral $ 2 ^ depth)
   writeSTRef rgRef rg'
   M.write v nwIx nwV
   M.write v neIx neV
   M.write v swIx swV  
   M.write v seIx seV
   M.write v mIx mV
   squareStep' rgRef dims (depth + 1) ((nwX,nwY),nwV) ((neX,neY),neV) ((swX,swY),swV) ((seX,seY),seV) (m,mV + (jitter * power)) v >>= return
  where
    nwIx = index2 dims (nwX,nwY)
    neIx = index2 dims (neX,neY)
    swIx = index2 dims (swX,swY)
    seIx = index2 dims (seX,seY)
    m    = (iavg [nwX,neX,swX,seX],iavg [nwY,neY,swY,seY])
    mIx  = index2 dims m
    mV   = avg [nwV,neV,swV,seV]


--squareStep' ::
--  (M.MVector v Float, RandomGen g) 
--    => STRef s g
--    -> (Width, Height)
--    -> Int
--    -> ((Int, Int), Float)
--    -> ((Int, Int), Float)
--    -> ((Int, Int), Float)
--    -> ((Int, Int), Float)
--    -> ((Int, Int), Float)
--    -> v s Float
--    -> ST s (v s Float)
squareStep' rgRef dims depth ((nwX,nwY),nwV) ((neX,neY),neV) ((swX,swY),swV) ((seX,seY),seV) ((mX,mY),mV) v
  | done      = return v
  | otherwise = do
      diamondStep' rgRef dims depth' ((nwX,nwY),nwV) ((nX,nY),nV) ((wX,wY),wV) ((mX,mY),mV) v >>=
        diamondStep' rgRef dims depth' ((nX,nY),nV) ((neX,neY),neV) ((mX,mY),mV) ((eX,eY),eV) >>=
        diamondStep' rgRef dims depth' ((wX,wY),wV) ((mX,mY),mV) ((swX,swY),swV) ((sX,sY),sV) >>=
        diamondStep' rgRef dims depth' ((mX,mY),mV) ((eX,eY),eV) ((sX,sY),sV) ((seX,seY),seV) >>=
        return
  where
    depth'     = depth + 1
    done       = (abs $ nwX - seX) * (abs $ nwY - seY) <= 1
    (nX,nY,nV) = (iavg [nwX,neX],iavg [nwY,neY],avg [nwV,neV])
    (eX,eY,eV) = (iavg [neX,seX],iavg [neY,seY],avg [neV,seV])
    (wX,wY,wV) = (iavg [nwX,swX],iavg [nwY,swY],avg [nwV,swV])
    (sX,sY,sV) = (iavg [swX,seX],iavg [swY,seY],avg [swV,seV])

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


main :: IO ()
main = do

  nwSeed <- randomRIO (0.0,1.0) :: IO Float
  neSeed <- randomRIO (0.0,1.0)
  swSeed <- randomRIO (0.0,1.0)
  seSeed <- randomRIO (0.0,1.0)
  rg     <- getStdGen

  let (w,h)   = (16,16)
  let dims    = (Width w,Height h)

  let x  = computePoints rg dims nwSeed neSeed swSeed seSeed
  let x' = computePoints' rg dims nwSeed neSeed swSeed seSeed  

  putStrLn $ show x
  putStrLn " --- "
  putStrLn $ show $ Repa.fromUnboxed (Z :. w :. h) x'
