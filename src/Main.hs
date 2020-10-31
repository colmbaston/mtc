module Main where

import AST
import Display
import CodeGen
import TAM

import System.Exit
import System.FilePath
import System.Environment
import Data.Maybe
import Control.Monad
import Control.Exception

main :: IO ()
main = do Args file mode <- validateArgs
          case mode of
            Compile -> readMT  file >>= compileMT >>= writeTAM (replaceExtension file ".tam")
            AST     -> readMT  file >>= putStrLn . astDisplay
            CST     -> readMT  file >>= putStrLn . cstDisplay
            RunMT   -> readMT  file >>= compileMT >>= runTAM
            RunTAM  -> readTAM file >>= runTAM

-- COMMAND-LINE ARGUMENT HANDLING

data Args = Args FilePath Mode
data Mode = Compile | AST | CST | RunMT | RunTAM

validateArgs :: IO Args
validateArgs = do args <- getArgs
                  case args of
                    [file]              -> validateFile file (Just Compile) (Just RunTAM)
                    [file, "--compile"] -> validateFile file (Just Compile)  Nothing
                    [file, "--run"]     -> validateFile file (Just RunMT)   (Just RunTAM)
                    [file, "--ast"]     -> validateFile file (Just AST)      Nothing
                    [file, "--cst"]     -> validateFile file (Just CST)      Nothing
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
               putStrLn "  --cst      print the concrete syntax tree of the program"
               putStrLn ""
               putStrLn "Modes available for TAM code (MT code will be implicitly compiled first):"
               putStrLn "  --run      execute a TAM program to its final stack (default)"
               exitFailure

-- FILE IO

readFileChecked :: FilePath -> IO String
readFileChecked file = catch (readFile file) (readFileError file)

readFileError :: FilePath -> IOError -> IO a
readFileError file _ =  putStr "error: failed to read " *> putStr file *> putStrLn " from the filesystem" *> exitFailure

writeFileChecked :: FilePath -> String -> IO ()
writeFileChecked file contents = catch (writeFile file contents) (writeFileError file)

writeFileError :: FilePath -> IOError -> IO a
writeFileError file _ = putStr "error: failed to write " *> putStr file *> putStrLn " to the filesystem" *> exitFailure

-- MODE HANDLING

readMT :: FilePath -> IO Program
readMT file = do src <- readFileChecked file
                 case parseProgram src of
                   Left  e -> print e *> exitFailure
                   Right p -> pure p

compileMT :: Program -> IO [TAM]
compileMT = maybe (putStrLn "error: failed to generate code for MT program (e.g. duplicate variable declaraions or use of an undeclared variable)" *> exitFailure) (pure . optimiseTAM) . codeGen

readTAM :: FilePath -> IO [TAM]
readTAM file = do src <- readFileChecked file
                  case parseTAM src of
                    Left e  -> print e *> exitFailure
                    Right p -> pure p

writeTAM :: FilePath -> [TAM] -> IO ()
writeTAM file code = writeFileChecked file (formatTAM code) *> putStr "TAM code written to " *> putStrLn file

runTAM :: [TAM] -> IO ()
runTAM code = exec code >>= flip when (putStrLn "error: could not complete exection (e.g. division by zero or stack underflow)") . isNothing
