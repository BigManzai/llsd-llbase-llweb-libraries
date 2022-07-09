{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  TestData - Part of the Test LLSD Utility
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable

module TestData where


import Network.Format.LLSD
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (UUID)
import Network.URI (URI, nullURI, parseURIReference)
import Test.QuickCheck
import System.Random


data LogItem =
    LogIn { liAgent:: UUID, liTime:: UTCTime, liFirstTime:: Bool, liReferer:: URI }
    | LogOut { liAgent:: UUID, liTime:: UTCTime, liAttachments:: Attachments }
    | LogMisc { liAgent:: UUID, liTime:: UTCTime, liThis:: Int, liThat:: Double }
    | LogError
    deriving (Eq, Show)

instance SData LogItem where
    toLLSD (LogIn a t ft r)  = llsd `with` "event" .= "login"
                                    `with` "agent" .= a
                                    `with` "time" .= t
                                    `with` "firsttime" .= ft
                                    `with` "referer" .= r
    toLLSD (LogOut a t ats)  = llsd `with` "event" .= "logout"
                                    `with` "agent" .= a
                                    `with` "time" .= t
                                    `with` "attachments" .= ats
    toLLSD (LogMisc a t x y) = llsd `with` "event" .= "misc"
                                    `with` "agent" .= a
                                    `with` "time" .= t
                                    `with` "this" .= x
                                    `with` "that" .= y
    toLLSD LogError          = undef

    fromLLSD l = case l `at` "event" of
        "login"   -> LogIn   (l `at` "agent") (l `at` "time")
                             (l `at` "firsttime") (l `at` "referer")
        "logout"  -> LogOut  (l `at` "agent") (l `at` "time")
                             (l `at` "attachments")
        "misc"    -> LogMisc (l `at` "agent") (l `at` "time")
                             (l `at` "this") (l `at` "that")
        _         -> LogError

type Attachments = [(String, UUID)]

instance SData Attachments where
    toLLSD = toLLSD . map entryToLLSD
        where entryToLLSD (s, u) = llsd `with` "point" .= s
                                        `with` "inv" .= u
    fromLLSD = map entryFromLLSD . fromLLSD
        where entryFromLLSD l = (l `at` "point", l `at` "inv")



data LogStats = LogStats {
                    lsCountIn, lsCountOut, lsCountMisc, lsCountError:: !Int,
                    lsAttachmentsHistogram:: !(M.Map Int Int)
                    }
    deriving (Eq, Show)

zeroStats :: LogStats
zeroStats = LogStats 0 0 0 0 M.empty

note :: LogItem -> LogStats -> LogStats
note (LogIn _ _ _ _) s = s { lsCountIn = lsCountIn s + 1 }
note (LogOut _ _ ats) s =
    s { lsCountOut = lsCountOut s + 1,
        lsAttachmentsHistogram =
            M.insertWith' (+) (length ats) 1 (lsAttachmentsHistogram s) }
                -- must use the strict version of insertWith

note (LogMisc _ _ _ _) s = s { lsCountMisc = lsCountMisc s + 1 }
note LogError          s = s { lsCountError = lsCountError s + 1 }



makeLogItems :: Int -> IO [LogItem]
makeLogItems n = getStdRandom makeEm
    where makeEm g0 = let (g1, g2) = split g0 in
                      (generate n g1 (arbitraryItems n), g2)


arbitraryItems :: Int -> Gen [LogItem]
arbitraryItems n = replicateM n arbitraryItem

arbitraryItem, arbitraryIn, arbitraryOut, arbitraryMisc :: Gen LogItem
arbitraryItem = oneof [ arbitraryIn, arbitraryOut, arbitraryMisc ]

arbitraryIn =
    liftM4 LogIn arbitraryUUID arbitraryTime arbitrary arbitraryURI
arbitraryOut =
    liftM3 LogOut arbitraryUUID arbitraryTime arbitraryAttachements
arbitraryMisc =
    liftM4 LogMisc arbitraryUUID arbitraryTime arbitrary arbitrary

arbitraryUUID :: Gen UUID
arbitraryUUID = (fst . random) `fmap` rand

arbitraryTime :: Gen UTCTime
arbitraryTime = (posixSecondsToUTCTime . realToFrac) `fmap` choose (0, century)
    where century :: Integer
          century = 50*365*24*60*60

arbitraryURI :: Gen URI
arbitraryURI = return $ fromMaybe nullURI
                        $ parseURIReference "http://example.com/"

arbitraryAttachements :: Gen Attachments
arbitraryAttachements = choose (0,22) >>= \n -> replicateM n arbAttach
    where arbAttach = do
            p <- elements points
            u <- arbitraryUUID
            return (p, u)
          points = [ "head", "neck", "spine", "pelvis" ]
            ++ [ side ++ "-" ++ part |
                    side <- [ "left", "right" ],
                    part <- [ "shoulder", "arm", "hand", "leg", "foot" ]]

