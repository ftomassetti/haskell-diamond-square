{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main
where
import Data.Word (Word8)
import qualified Data.Sequence as S
import System.Random
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.BMP
import HeightMap.Base (reify,heightMap,heightMap',unitHeightMap,unitHeightMap',getHeight,HeightMap,Point,float2bytes)
import HeightMap.Mesh ()
import System.Console.GetOpt
import System.Environment
import System.IO
import Data.Maybe ( fromMaybe, isJust, fromJust, isNothing )
import Control.Monad
import qualified PBHeightmap.HeightMap as PH
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import qualified Data.ByteString.Lazy as ByteString (
       readFile, writeFile, length)

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
    [ Option ['s']     ["seed"]       (OptArg seedOption      "SEED"  ) "use SEED",
      Option ['w']     ["width"]      (OptArg widthOption     "WIDTH" ) "set map WIDTH",
      Option ['h']     ["height"]     (OptArg heightOption    "HEIGHT") "set map HEIGHT",
      Option ['b']     ["bitmap"]     (OptArg bmpOption       "FILE"  ) "output bitmap FILE",
      Option ['m']     ["heightmap"]  (OptArg heightmapOption "FILE"  ) "output heightmap FILE" ]

seedOption, widthOption, heightOption, bmpOption, heightmapOption :: Maybe String -> Options -> Options
seedOption      val opts
    | isNothing val = opts
    | otherwise          = opts { optSeed      = Just seed }
                           where seed :: Int = read $ fromJust val
widthOption     val opts
    | isNothing val = opts
    | otherwise     = opts { optWidth = read $ fromJust val}
heightOption    val opts
    | isNothing val = opts
    | otherwise     = opts { optHeight = read $ fromJust val}
bmpOption       val opts = opts { optBmp       = val }
heightmapOption val opts = opts { optHeightmap = val }

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
     case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: <To be written>"

-- Misc

getSeed :: Options -> IO Int
getSeed opts = if isJust $ optSeed opts
               then return (fromJust $ optSeed opts)
               else randomIO

saveHeightMap :: Int -> Int -> HeightMap (Point Float) -> FilePath -> IO ()
saveHeightMap width height cells filename = ByteString.writeFile filename $ messagePut protoHm
                                            where protoHm = convertToPH width height cells

convertToPH :: Int -> Int -> HeightMap (Point Float) -> PH.HeightMap
convertToPH width height hm = PH.HeightMap (fromIntegral width) (fromIntegral height) (convertToSeqFloat hm)

convertToSeqFloat :: HeightMap (Point Float) -> S.Seq Float
convertToSeqFloat hm = S.fromList $ map getHeight (R.toList hm)

-- Main

main :: IO ()
main = do
  args <- getArgs

  putStrLn "Diamond square generator"
  putStrLn "------------------------"

  (opts,rest) <- compilerOpts args

  when (not $ null rest) $ ioError (userError $ "Invalid arguments: " ++ show rest)

  when (isNothing (optBmp opts) && isNothing (optHeightmap opts)) $ ioError (userError "Nothing to do, please choose -b or -m")

  let width  = optWidth opts
  let height = optHeight opts
  when (width  < 1 || width  > 8192) $ ioError (userError "Invalid width. Width should be in [1,8192]")
  when (height < 1 || height > 8192) $ ioError (userError "Invalid height. Height should be in [1,8192]")

  seed <- getSeed opts
  putStrLn      $ "Seed      : " ++ (show seed)
  setStdGen $ mkStdGen seed
  rg <- getStdGen

  putStrLn      $ "Width     : " ++ show width
  putStrLn      $ "Height    : " ++ show height

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

  let heightMap = unitHeightMap rg (width,height) seed1 seed2 seed3 seed4

  let hm = reify $ R.map (\p -> float2bytes $ getHeight p)
                 $ heightMap

  when (isJust (optHeightmap opts)) $ do
    let filename = fromJust ( optHeightmap opts )
    saveHeightMap width height heightMap filename

  when (isJust (optBmp opts)) $ do
    let filename = fromJust ( optBmp opts )
    writeImageToBMP filename hm
