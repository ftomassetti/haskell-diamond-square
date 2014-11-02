import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Cmd (rawSystem)
import System.FilePath ((</>))

-- Hook .proto module extension to be processed with hprotoc
main = do putStrLn "[Custom build (generate Google Protocol Buffers code)]"
          let hooks = simpleUserHooks
              protoc  = ("proto", protoC)
           in defaultMainWithHooks hooks { hookedPreProcessors = protoc:knownSuffixHandlers  }

protoC :: BuildInfo -> LocalBuildInfo -> PreProcessor
protoC build local = PreProcessor {
  platformIndependent = True,
  runPreProcessor = \(inPath, inFile) (outPath, outFile) verbosity -> do
    putStrLn $ "Running preprocessor on " ++ inPath
    notice verbosity (inPath </> inFile ++ " is being preprocessed to " ++
                      outPath </> outFile ++ " and a few more maybe")
    rawSystem "hprotoc" ["--haskell_out=" ++ outPath, inPath </> inFile]
    return ()
  }