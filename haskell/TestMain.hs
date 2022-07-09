-- |
-- Module      :  Main - Test LLSD Utility
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable

module Main where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (foldl')
import Data.Serialize
import Data.Word
import Network.Format.LLSD
import qualified Network.Format.LLSD.Test as Test
import qualified Network.Format.LLSD.TestIDL as TestIDL
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import TestData

data Operation = Test | Help | Generate | Read

data Mode = Mode {
    modeOperation   :: Operation,
    modeCount       :: Int,
    modeGroup       :: Int,
    modeReportEvery :: Int,
    modeQuiet       :: Bool,
    modeParse       :: B.ByteString -> Either String LLSD,
    modeFormat      :: LLSD -> L.ByteString,
    modeDataFile    :: Maybe String
    }

defaultMode :: Mode
defaultMode = Mode {
    modeOperation   = Help,
    modeCount       = 0,
    modeGroup       = 1,
    modeReportEvery = 1000,
    modeQuiet       = False,
    modeParse       = parseXML,
    modeFormat      = formatXML,
    modeDataFile    = Nothing
    }

setOperation :: Operation -> Mode -> Mode
setOperation op mode = mode { modeOperation = op }

setTest, setHelp, setRead :: Mode -> Mode
setGenerate :: String -> Mode -> Mode
setTest       = setOperation Test
setHelp       = setOperation Help
setGenerate n = setOperation Generate . setCount n
setRead       = setOperation Read

setCount, setGroup, setReportEvery, setDataFile :: String -> Mode -> Mode
setCount n mode       = mode { modeCount = read n }
setGroup n mode       = mode { modeGroup = read n }
setReportEvery n mode = mode { modeReportEvery = read n }
    -- FIXME: should do something better than read, which can error
setDataFile f mode    = mode { modeDataFile = Just f }

setXML, setBinary, setQuiet :: Mode -> Mode
setXML mode    = mode { modeParse = parseXML,    modeFormat = formatXML }
setBinary mode = mode { modeParse = parseBinary, modeFormat = formatBinary }
setQuiet mode  = mode { modeQuiet = True }

options :: [OptDescr (Mode -> Mode)]
options = [
    Option ['t'] ["test", "tests"]      (NoArg setTest)
                                        "run unit tests and checks",

    Option ['g'] ["gen", "generate"]    (ReqArg setGenerate "N")
                                        "generate N packets of LLSD output",
    Option ['c'] ["count"]              (ReqArg setGroup "M")
                                        "generate M records per packet",

    Option ['r'] ["read"]               (NoArg setRead)
                                        "read packets of LLSD",
    Option ['e'] ["every"]              (ReqArg setReportEvery "N")
                                        "report stats every N packets",

    Option ['b'] ["binary"]             (NoArg setBinary)
                                        "binary encoded LLSD",
    Option ['x'] ["xml", "XML"]         (NoArg setXML)
                                        "XML encoded LLSD",

    Option ['d'] ["data"]               (ReqArg setDataFile "F")
                                        "read/write data file F",
    Option ['q'] ["quiet"]              (NoArg setQuiet)
                                        "supress message output",

    Option ['?'] ["help"]               (NoArg setHelp)
                                        "print help summary"
    ]

usage :: [String] -> Handle -> IO ()
usage errs h = do
        progName <- getProgName
        forM_ errs (hPutStr h)
        hPutStr h $ usageInfo (header progName) options
    where header progName = "Usage: " ++ progName ++ " [options]"

usageFailure :: [String] -> IO a
usageFailure errs = usage errs stderr >> exitFailure

parseOptions :: IO (Mode, [String])
parseOptions = do
    argv <- getArgs
    case getOpt Permute options argv of
        (opts, args,   []) -> return (foldl' (flip ($)) defaultMode opts, args)
        (   _,    _, errs) -> usageFailure errs

runtimeFailure :: [String] -> IO a
runtimeFailure msgs = do
    forM_ msgs (\m -> hPutStrLn stderr m)
    exitFailure





hEncode :: (Serialize a) => Handle -> a -> IO ()
hEncode h = B.hPut h . encode

hDecode :: (Serialize a) => Handle -> String -> Int -> IO a
hDecode h msg n = do
    buf <- B.hGet h n
    case decode buf of
        Left err -> runtimeFailure [msg, err]
        Right a  -> return a

hPutInt32 :: (Integral a) => Handle -> a -> IO ()
hPutInt32 h i = hEncode h (fromIntegral i :: Word32)
hGetInt32 :: Handle -> String -> IO Int
hGetInt32 h msg = (hDecode h msg 4 :: IO Word32) >>= return . fromIntegral


report :: (Show a) => Mode -> Handle -> a -> IO ()
report mode h a =
    when (not $ modeQuiet mode) $ hPutStrLn h (show a)

reportCount :: Mode -> Int -> IO ()
reportCount mode i =
        when (i `mod` (modeReportEvery mode) == 0) $ report mode stderr i

generateData :: Mode -> Handle -> IO ()
generateData mode h = do
    let count = modeCount mode
        group = modeGroup mode
        formatter = modeFormat mode
    hSetBinaryMode h True
    -- write format indicator
    hPutInt32 h count
    forM_ [1..count] $ \i -> do
        items <- makeLogItems group
        let bs = (formatter $ toLLSD items)
        hPutInt32 h $ L.length bs
        L.hPut h bs
        reportCount mode i

readData :: Mode -> Handle -> IO ()
readData mode h = do
    hSetBinaryMode h True
    -- read format indicator
    count <- hGetInt32 h "packet count"
    foldM readPacket zeroStats [1..count] >>= report mode stdout
    where readPacket :: LogStats -> Int -> IO LogStats
          readPacket s0 i = s0 `seq` do -- this force of s0 is important
                bs <- hGetInt32 h "length" >>= B.hGet h
                reportCount mode i
                case (modeParse mode $ bs) of
                    Right l -> return $ foldl' (flip note) s0 (fromLLSD l :: [LogItem])
                    Left err -> runtimeFailure ["packet decode error", err]


runAllTests :: IO()
runAllTests = do
    Test.runTests
    Test.runChecks
    TestIDL.runTests
    TestIDL.runChecks
    

withDataHandle :: IOMode -> Handle -> Mode -> (Handle -> IO a) -> IO a
withDataHandle iomode defHandle mode act =
    maybe (act defHandle) (\f -> withFile f iomode act) $ modeDataFile mode

withInputHandle :: Mode -> (Handle -> IO a) -> IO a
withInputHandle = withDataHandle ReadMode stdin

withOutputHandle :: Mode -> (Handle -> IO a) -> IO a
withOutputHandle = withDataHandle WriteMode stdout
    

main :: IO ()
main = do
    (mode, _files) <- parseOptions
    case modeOperation mode of
        Help     -> usage [] stdout
        Test     -> runAllTests
        Generate -> withOutputHandle mode $ generateData mode
        Read     -> withInputHandle mode $ readData mode



