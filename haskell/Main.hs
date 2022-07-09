-- |
-- Module      :  Main - LLSD Utility
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable

module Main where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.List (foldl')
import Data.Maybe
import Network.Format.LLSD
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.PrettyPrint.HughesPJ (render)


data Mode = Mode {
    modeHelp :: Bool,
    modeInput :: Maybe (B.ByteString -> Either String LLSD),
    modeOutput :: Maybe (LLSD -> L.ByteString)
    }

defaultMode :: Mode
defaultMode = Mode {
    modeHelp = False,
    modeInput = Nothing,
    modeOutput = Nothing
    }



options :: [OptDescr (Mode -> Mode)]
options = [
    Option ['b'] ["in-binary"]  (NoArg setInBinary) "read binary encoded LLSD",
    Option ['e'] ["in-example"] (NoArg setInExample) "use example LLSD as input",
    Option ['x'] ["in-xml"]     (NoArg setInXML) "read XML encoded LLSD",

    Option ['B'] ["out-binary"] (NoArg setOutBinary) "write XML encoded ouptut",
    Option ['P'] ["out-pretty"] (NoArg setOutPretty) "write pretty print output",
    Option ['S'] ["out-show"]   (NoArg setOutShow) "write show output",
    Option ['V'] ["out-valid"]  (NoArg setOutValid) "write only if LLSD is valid",
    Option ['X'] ["out-xml"]    (NoArg setOutXML) "write XML encoded ouptut",

    Option ['?'] ["help"] (NoArg setHelp) "print help summary"
    ]
    where setInput f mode  = mode { modeInput = Just f }
          setOutput f mode = mode { modeOutput = Just f }
          setHelp mode     = mode { modeHelp = True }

          setInExample = setInput $ const $ Right (llsd `with` "name" .= "Amy")
          setInXML = setInput parseXML
          setInBinary = setInput parseBinary

          setOutPretty = setOutput $ U.fromString . render . prettyLLSD
          setOutShow = setOutput $ U.fromString . show
          setOutValid = setOutput $ const $ U.fromString "valid LLSD"
          setOutXML = setOutput formatXML
          setOutBinary = setOutput formatBinary

usage :: [String] -> Handle -> IO ()
usage errs h =
    do
        progName <- getProgName
        forM_ errs (hPutStr h)
        hPutStr h $ usageInfo (header progName) options
    where header progName = "Usage: " ++ progName ++ " [options] [files]"

usageFailure :: [String] -> IO a
usageFailure errs = usage errs stderr >> exitFailure

parseOptions :: IO (Mode, [String])
parseOptions =
    do
        argv <- getArgs
        case getOpt Permute options argv of
            (opts, args,   []) -> return (foldl' (flip ($)) defaultMode opts, args)
            (   _,    _, errs) -> usageFailure errs




process :: Mode -> B.ByteString -> IO ()
process mode input =
    case (modeInput mode, modeOutput mode) of
        (Just inF, Just outF) ->
            case inF input of
                Left err -> hPutStrLn stderr err
                Right v  -> L.putStr $ outF v
        (inM, outM) ->
            usageFailure (errIfMissing "No input mode selected\n" inM
                       ++ errIfMissing "No output mode selected\n" outM)
    where errIfMissing msg f = maybe [msg] (const []) f


processHandle :: Mode -> Handle -> IO ()
processHandle mode h = B.hGetContents h >>= process mode

processFile :: Mode -> String -> IO ()
processFile mode path = B.readFile path >>= process mode

main :: IO ()
main = do
    (mode, files) <- parseOptions
    when (modeHelp mode) $ do
        usage [] stdout
        exitSuccess
    if null files
        then processHandle mode stdin
        else forM_ files $ processFile mode


