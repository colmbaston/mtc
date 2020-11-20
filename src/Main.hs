module Main where

import AST
import Parser
import TypeCheck
import CodeGen
import TAM

import System.Exit
import System.FilePath
import System.Environment
import Control.Exception

main :: IO ()
main = do Args file mode <- validateArgs
          case mode of
            Compile -> readMT  file >>= compileMT >>= writeTAM (replaceExtension file ".tam")
            AST     -> readMT  file >>= putStrLn . astDisplay
            RunMT   -> readMT  file >>= compileMT >>= runTAM
            RunTAM  -> readTAM file >>= runTAM

-- COMMAND-LINE ARGS

data Args = Args FilePath Mode
data Mode = Compile | AST | RunMT | RunTAM

validateArgs :: IO Args
validateArgs = do args <- getArgs
                  case args of
                    [file]              -> validateFile file (Just Compile) (Just RunTAM)
                    [file, "--compile"] -> validateFile file (Just Compile)  Nothing
                    [file, "--run"]     -> validateFile file (Just RunMT)   (Just RunTAM)
                    [file, "--ast"]     -> validateFile file (Just AST)      Nothing
                    _                   -> usageFail

validateFile :: FilePath -> Maybe Mode -> Maybe Mode -> IO Args
validateFile file mt tam = maybe usageFail
                                (pure . Args file)
                                (case takeExtension file of
                                   ".mt"  -> mt
                                   ".tam" -> tam
                                   _      -> Nothing)

usageFail :: IO a
usageFail = do p <- getProgName
               putStrLn "MiniTriangle Compiler"
               putStrLn ""
               putStrLn "Usage:"
               putStr   "  " *> putStr p *> putStrLn " file/path.mt  [MT  mode]"
               putStr   "  " *> putStr p *> putStrLn " file/path.tam [TAM mode]"
               putStrLn ""
               putStrLn "Modes available for MT code:"
               putStrLn "  --compile  compile the program to TAM code and write it to a file (default)"
               putStrLn "  --ast      print the abstract syntax tree of the program"
               putStrLn ""
               putStrLn "Modes available for TAM code (MT code will be implicitly compiled first):"
               putStrLn "  --run      execute a TAM program to its final stack (default)"
               exitFailure

-- FILE IO

readFileChecked :: FilePath -> IO String
readFileChecked file = catch (readFile file) handler
  where
    handler :: IOError -> IO a
    handler _ = putStr "file error: failed to read " *> putStr file *> putStrLn " from the filesystem" *> exitFailure

writeFileChecked :: FilePath -> String -> IO ()
writeFileChecked file contents = catch (writeFile file contents) handler
  where
    handler :: IOError -> IO a
    handler _ = putStr "file error: failed to write " *> putStr file *> putStrLn " to the filesystem" *> exitFailure

-- MODE HANDLING

readMT :: FilePath -> IO Program
readMT file = do src <- readFileChecked file
                 case parseProgram src of
                   Left  e -> print e *> exitFailure
                   Right p -> pure p

compileMT :: Program -> IO [TAM]
compileMT p = case typeCheck p of
                Left   e -> print e *> exitFailure
                Right () -> case optimiseTAM <$> codeGen p of
                              Left   e -> print e *> exitFailure
                              Right is -> pure is

readTAM :: FilePath -> IO [TAM]
readTAM file = do src <- readFileChecked file
                  case parseTAM src of
                    Left   e -> print e *> exitFailure
                    Right is -> pure is

writeTAM :: FilePath -> [TAM] -> IO ()
writeTAM file code = writeFileChecked file (formatTAM code) *> putStr "TAM code written to " *> putStrLn file

runTAM :: [TAM] -> IO ()
runTAM code = do r <- exec code
                 case r of
                   Left  e -> print e *> exitFailure
                   Right _ -> pure ()
