{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ghci -fno-ghci-sandbox DiamondSquare.hs

module Main
where
import Data.Word (Word8)
import System.Random
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.BMP
import HeightMap.Base (reify,heightMap,heightMap',unitHeightMap,unitHeightMap',getHeight)
import HeightMap.Mesh ()
import System.Console.GetOpt
import System.Environment
import System.IO
import Data.Maybe ( fromMaybe, isJust, fromJust, isNothing )
import Control.Monad

--------------------------------------------------------------------------------

-- Command line options processing

data Options = Options {
    optBmp       :: Maybe FilePath,
    optHeightmap :: Maybe FilePath,
    optSeed      :: Maybe Int,
    optWidth     :: Int,
    optHeight    :: Int
} deriving Show

defaultOptions   = Options {
    optBmp       = Nothing,
    optHeightmap = Nothing,
    optSeed      = Nothing,
    optWidth     = 512,
    optHeight    = 512
}

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['s']     ["seed"]       (OptArg seedOption      "SEED")  "use SEED",
      Option ['b']     ["bitmap"]     (OptArg bmpOption       "FILE")  "output bitmap FILE",
      Option ['m']     ["heightmap"]  (OptArg heightmapOption "FILE")  "output heightmap FILE" ]

seedOption, bmpOption, heightmapOption :: Maybe String -> Options -> Options
seedOption      val opts
    | isNothing val = opts
    | otherwise          = opts { optSeed      = Just seed }
                           where seed :: Int = read $ fromJust val
bmpOption       val opts = opts { optBmp       = val }
heightmapOption val opts = opts { optHeightmap = val }

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
     case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: <To be written>"

-- Misc

float2bytes :: Float -> (Word8,Word8,Word8)
float2bytes v = (b,b,b) where b = (floor $ 255.0 * v) :: Word8

getSeed :: Options -> IO Int
getSeed opts = if isJust $ optSeed opts
               then return (fromJust $ optSeed opts)
               else randomIO

-- Main

main :: IO ()
main = do
  args <- getArgs

  putStrLn "Diamond square generator"
  putStrLn "------------------------"

  (opts,errors) <- compilerOpts args

  when (isNothing (optBmp opts) && isNothing (optHeightmap opts)) $ ioError (userError "Nothing to do, please choose -b or -m")

  seed <- getSeed opts
  putStrLn      $ "Seed      : " ++ (show seed)
  setStdGen $ mkStdGen seed
  rg <- getStdGen

  if isJust $ optBmp opts
  then putStrLn $ "Bitmap    : '" ++ fromJust ( optBmp opts ) ++ "'"
  else putStrLn $ "Bitmap    : <not produced>"

  if isJust $ optHeightmap opts
  then putStrLn $ "Heightmap : '" ++ fromJust ( optHeightmap opts ) ++ "'"
  else putStrLn $ "Heightmap : <not produced>"

  seed1 <- randomRIO (0.0, 1.0) :: IO Float
  seed2 <- randomRIO (0.0, 1.0) :: IO Float
  seed3 <- randomRIO (0.0, 1.0) :: IO Float
  seed4 <- randomRIO (0.0, 1.0) :: IO Float

  let hm = reify $ R.map (\p -> float2bytes $ getHeight p) 
                 $ unitHeightMap rg (240,240) seed1 seed2 seed3 seed4

  when (isJust (optBmp opts)) $ do
    let filename = fromJust ( optBmp opts )
    writeImageToBMP filename hm
