{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}

-- ghci -fno-ghci-sandbox DiamondSquare.hs

module DiamondSquare
where


import Control.Monad.ST
import Data.Array.ST

import Data.Word
import Data.List
import qualified Data.DList as DList
import Data.Array.Repa hiding (map, (++))
import qualified Data.Array.Repa as Repa
import Control.Monad.State
import System.Random
import Debug.Trace
import Text.Printf
import Data.Array.Repa.IO.BMP
import Graphics.Gloss.Raster.Array


newtype Width  = Width Int deriving (Eq,Show,Ord)
newtype Height = Height Int deriving (Eq,Show,Ord)


--boxST :: 
--     (Width, Height)
--  -> ((Int, Int), Float)
--  -> ((Int, Int), Float)
--  -> ((Int, Int), Float)
--  -> ((Int, Int), Float)
--  -> GHC.Arr.Array Int Float
boxST dims@(Width w,Height h) (nw,nwV) (ne,neV) (sw,swV) (se,seV) =
  runSTArray $ do 
    newArray (0,(w * h)-1) 0.0 >>= 
      diamondStep dims 0 (nw,nwV) (ne,neV) (sw,swV) (se,seV) >>= 
        return 


diamondStep ::
  (MArray s Float m)
    => (Width, Height)
    -> Int
    -> ((Int,Int), Float)
    -> ((Int,Int), Float)
    -> ((Int,Int), Float)
    -> ((Int,Int), Float)
    -> s Int Float
    -> m (s Int Float)
diamondStep dims depth ((nwX,nwY),nwV) ((neX,neY),neV) ((swX,swY),swV) ((seX,seY),seV) arr
  | done      = return arr
  | otherwise = do
     writeArray arr nwIx nwV
     writeArray arr neIx neV
     writeArray arr swIx swV  
     writeArray arr seIx seV
     writeArray arr mIx mV
     squareStep dims
                (depth + 1) 
                ((nwX,nwY),nwV) 
                ((neX,neY),neV) 
                ((swX,swY),swV) 
                ((seX,seY),seV) 
                (m,mV) 
                arr >>= return
  where
    done = neX >= nwX && swY >= nwY
    nwIx = index2 dims (nwX,nwY)
    neIx = index2 dims (neX,neY)
    swIx = index2 dims (swX,swY)
    seIx = index2 dims (seX,seY)
    m    = (iavg [nwX,neX,swX,seX],iavg [nwY,neY,swY,seY])
    mIx  = index2 dims m
    mV   = avg [nwV,neV,swV,seV]


squareStep ::
  (MArray s Float m) 
    => (Width, Height)
    -> Int
    -> ((Int,Int), Float)
    -> ((Int,Int), Float)
    -> ((Int,Int), Float)
    -> ((Int,Int), Float)
    -> ((Int,Int), Float)
    -> s Int Float
    -> m (s Int Float)
squareStep dims depth ((nwX,nwY),nwV) ((neX,neY),neV) ((swX,swY),swV) ((seX,seY),seV) ((mX,mY),mV) arr
  | done      = return arr
  | otherwise = do
    diamondStep dims depth ((nwX,nwY),nwV) ((nX,nY),nV) ((wX,wY),wV) ((mX,mY),mV) arr >>=
     diamondStep dims depth ((nX,nY),nV) ((neX,neY),neV) ((mX,mY),mV) ((eX,eY),eV) >>=
     diamondStep dims depth ((wX,wY),wV) ((mX,mY),mV) ((swX,swY),swV) ((sX,sY),sV) >>=
     diamondStep dims depth ((mX,mY),mV) ((eX,eY),eV) ((sX,sY),sV) ((seX,seY),seV) >>=
     return
  where
    depth'     = depth + 1
    done       = neX >= nwX && swY >= nwY
    (nX,nY,nV) = (iavg [nwX,neX],iavg [nwY,neY],avg [nwV,neV])
    nIx        = index2 dims (nX,nY)
    (eX,eY,eV) = (iavg [neX,seX],iavg [neY,seY],avg [neV,seV])
    eIx        = index2 dims (eX,eY)
    (wX,wY,wV) = (iavg [nwX,swX],iavg [nwY,swY],avg [nwV,swV])
    wIx        = index2 dims (wX,wY)
    (sX,sY,sV) = (iavg [swX,seX],iavg [swY,seY],avg [swV,seV])
    sIx        = index2 dims (sX,sY)

--------------------------------------------------------------------------------


box :: 
  (RandomGen g, MonadState g m) 
    => (Width,Height) -- ^ Grid (width,height) in pixels
    -> Int
    -> (Int,Float)    -- ^ NW corner
    -> (Int,Float)    -- ^ NE corner
    -> (Int,Float)    -- ^ SW corner
    -> (Int,Float)    -- ^ SE corner
    -> m (DList.DList (Int,Float))
box dims@(Width width,Height height) depth (nw,nwI) (ne,neI) (sw,swI) (se,seI)
  | done = 
    return $ DList.singleton (index2 dims midpoint, mI)
  | otherwise = do
      g <- get
      let (jitter,g')  = randomR (-1.0,1.0 :: Float) g
      let (mX,mY) = midpoint
      let (n,nI)  = ((mX, iavg [nwY,neY]), clamp 0 1 $ avg [nwI,neI] + (jitter * power))
      let (e,eI)  = ((iavg [neX,seX], mY), clamp 0 1 $ avg [neI,seI] + (jitter * power))
      let (w,wI)  = ((iavg [nwX,swX], mY), clamp 0 1 $ avg [nwI,swI] + (jitter * power))
      let (s,sI)  = ((mX, iavg [swY,seY]), clamp 0 1 $ avg [swI,seI] + (jitter * power))
      let mI'     = clamp 0 1 $ mI + (jitter * power)
      put g'

      -- NW --
      b1 <- box dims depth' (index2 dims (nwX,nwY),nwI) (index2 dims n,nI) (index2 dims w,wI) (index2 dims (mX,mY),mI')
      -- NE --
      b2 <- box dims depth' (index2 dims n,nI) (index2 dims (neX,neY),neI) (index2 dims (mX,mY),mI') (index2 dims e,eI)
      -- SW --    
      b3 <- box dims depth' (index2 dims w,wI) (index2 dims (mX,mY),mI') (index2 dims (swX,swY),swI) (index2 dims s,sI)
      -- SE --
      b4 <- box dims depth' (index2 dims (mX,mY),mI') (index2 dims e,eI) (index2 dims s,sI) (index2 dims (seX,seY),seI)

      return $ b1 `DList.append` b2 `DList.append` b3 `DList.append` b4
  where
    depth'    = depth + 1
    power     = 1.0 / (fromIntegral depth)
    boxWidth  = abs $ nwX - neX
    boxHeight = abs $ nwY - swX
    (nwX,nwY) = index1 dims nw
    (neX,neY) = index1 dims ne
    (swX,swY) = index1 dims sw
    (seX,seY) = index1 dims se
    midpoint  = (iavg [nwX,neX,swX,seX], iavg [nwY,neY,swY,seY])
    mI        = avg [nwI,neI,swI,seI]
    done      = (abs $ nwX - neX) <= 1 && (abs $ nwY - swY) <= 1


clamp :: Ord a => a -> a -> a -> a
clamp lo hi n = min (max n lo) hi


avg :: Fractional a => [a] -> a
avg xs = sum xs / (fromIntegral $ length xs)


iavg :: [Int] -> Int
iavg xs = floor $ sum (map fromIntegral xs) / (fromIntegral $ length xs)


index2 :: (Width,Height) -> (Int,Int) -> Int
index2 (Width w,Height h) (i,j) = (i * (w + 1)) + j


index1 :: (Width,Height) -> Int -> (Int,Int)
index1 (Width w,Height h) i = (i `div` w',i `mod` w') where w' = w + 1


computePoints :: 
  (RandomGen g)
    => g
    -> (Width,Height)
    -> Float
    -> Float
    -> Float
    -> Float
    -> [Float]
computePoints rg dims@(Width w,Height h) nwSeed neSeed swSeed seSeed = 
  map (avg . map snd) $ groupBy groupEq $ sortBy compareEq ps
  where
    nw = (index2 dims (0,0), nwSeed)
    ne = (index2 dims (w,0), neSeed)
    sw = (index2 dims (0,h), swSeed)
    se = (index2 dims (w,h), seSeed)
    groupEq i j   = fst i == fst j
    compareEq i j = fst i `compare` fst j
    ps            = DList.toList $ (evalState $ box dims 1 nw ne sw se) rg


makeArray :: (Width,Height) -> [Float] -> Array U DIM2 Float
makeArray (Width w,Height h) = fromListUnboxed (Z :. (w::Int) :. (h::Int))


toColor :: Array U DIM2 Float -> Array D DIM2 Color
toColor = Repa.map (\v -> rgb v v v) 


toWord8 :: Array U DIM2 Float -> Array U DIM2 (Word8,Word8,Word8)
toWord8 = computeUnboxedS . Repa.map (\v -> (byte v,byte v,byte v))
  where
    byte v = floor $ v * 255 :: Word8


main :: IO ()
main = do

  nwSeed <- randomRIO (0.0,1.0)
  neSeed <- randomRIO (0.0,1.0)
  swSeed <- randomRIO (0.0,1.0)
  seSeed <- randomRIO (0.0,1.0)
  rg     <- getStdGen

  let (w,h)   = (16,16)
  let dims    = (Width w,Height h)

  let x = boxST dims ((0,0),nwSeed) ((w,0),neSeed) ((0,h),swSeed) ((w,h),seSeed)
  putStrLn $ show x

  --let points  = computePoints rg dims nwSeed neSeed swSeed seSeed
  --let display = InWindow "Diamond-Square" (w,h) (0,0)

  --putStrLn $ show $ length points
  --forM_ points $ \p -> do
  --  putStrLn $ show p

  --let arr = makeArray dims points

  --putStrLn $ show arr

  --writeImageToBMP "/tmp/diamond_square.bmp" $ toWord8 arr
  --animateArray display (w,h) (const $ toColor arr)
