{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}

module DiamondSquare
where

import Control.Monad.State
import System.Random
--import Graphics.Gloss
import Debug.Trace 
import Text.Printf
import Data.List


box :: 
  (MonadState StdGen m) 
  => (Int,Int)
  -> (Int,Float)
  -> (Int,Float)
  -> (Int,Float)
  -> (Int,Float)
  -> m [(Int,Float)]
box d@(w,h) (nw,nwI) (ne,neI) (sw,swI) (se,seI)
  | done      = return $ [(ix2 d midpoint, avg [nwI,neI,swI,seI])]
  | otherwise = do
      g <- get
      let jitterRange    = (-5 :: Int,5 :: Int) 
      let (jitterX,g')   = randomR jitterRange g
      let (jitterY,g'')  = randomR jitterRange g'
      let ((mX',mY'),mI) = (midpoint, avg [nwI,neI,swI,seI])
      let (mX,mY)        = (mX', mY')
      let ((nX,nY),nI)   = ((mX, avgI [nwY,neY]), avg [nwI,neI])
      let ((eX,eY),eI)   = ((avgI [neX,seX], mY), avg [neI,seI])
      let ((wX,wY),wI)   = ((avgI [nwX,swX], mY), avg [nwI,swI])
      let ((sX,sY),sI)   = ((mX, avgI [swY,seY]), avg [swI,seI])
      put g''
      b1 <- box d (ix2 d (nwX,nwY), nwI) (ix2 d (nX,nY), nI) (ix2 d (wX,wY), wI) (ix2 d (mX,mY), mI) -- NW
      b2 <- box d (ix2 d (nX,nY), nI) (ix2 d (neX,neY), neI) (ix2 d (mX,mY), mI) (ix2 d (eX,eY), eI) -- NE
      b3 <- box d (ix2 d (wX,wY), wI) (ix2 d (mX,mY), mI) (ix2 d (swX,swY), swI) (ix2 d (sX,sY), sI) -- SW
      b4 <- box d (ix2 d (mX,mY), mI) (ix2 d (eX,eY), eI) (ix2 d (sX,sY), sI) (ix2 d (seX,seY), seI) -- SE
      return $ b1 ++ b2 ++ b3 ++ b4
  where
    (nwX,nwY) = ix1 (w,h) nw
    (neX,neY) = ix1 (w,h) ne
    (swX,swY) = ix1 (w,h) sw
    (seX,seY) = ix1 (w,h) se

    avg xs  = sum xs / (fromIntegral $ length xs)
    avgI xs = floor $ sum (map fromIntegral xs) / (fromIntegral $ length xs)

    done = trace (printf "NW = (%d,%d), SE = (%d,%d)" nwX nwY seX seY) $ 
      (seX - nwX) <= 1 && (seY - nwY) <= 1

    midpoint = (avgI [nwX,neX,swX,seX], avgI [nwY,neY,swY,seY])


ix2 :: (Int,Int) -> (Int,Int) -> Int
ix2 (w,h) (i,j) = (i * (w + 1)) + j


ix1 :: (Int,Int) -> Int -> (Int,Int)
ix1 (w,h) i = (i `div` w',i `mod` w') where w' = w + 1


main :: IO ()
main = do
  let (minI,maxI) = (0.0 :: Float,1.0 :: Float)
  seed1 <- randomRIO (minI,maxI)
  seed2 <- randomRIO (minI,maxI)
  seed3 <- randomRIO (minI,maxI)
  seed4 <- randomRIO (minI,maxI)
  g     <- getStdGen
  let (w,h)   = (6,4)
  let nw      = (ix2 (w,h) (0,0), seed1)
  let ne      = (ix2 (w,h) (w,0), seed2)
  let sw      = (ix2 (w,h) (0,h), seed3)
  let se      = (ix2 (w,h) (w,h), seed4)
  --forM_ [(i,j) | i <- [0..w], j <- [0..h]] $ \t -> do
  --  let k = ix2 (w,h) t
  --  let k' = ix1 (w,h) k
  --  putStrLn $ (show t) ++ " -- " ++ (show k) ++ " -- " ++ (show k')
  let points  = sortBy (\x y -> compare (fst x) (fst y)) $ (evalState $ box (w,h) nw ne sw se) g
  putStrLn $ show $ length points
  forM_ points $ \p@(i,d) -> do
    putStrLn $ printf "%s i = %s" (show p) (show $ (ix1 (w,h) i))

