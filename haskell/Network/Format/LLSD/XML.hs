-- |
-- Module      :  Network.Format.LLSD.XML
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable

module Network.Format.LLSD.XML (
    parseXML,
    formatXML,
    )
    where

import Codec.Binary.Base64 as Base64
import Control.Monad.Error
import Data.Char (isSpace)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (foldl')
import qualified Data.Map as Map
-- import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Network.Format.LLSD.Internal
import Network.URI (URI)
import qualified Text.XML.Expat.Tree as XML
import qualified Text.XML.Expat.Format as XML

type XText = B.ByteString
toXText :: String -> XText
toXText = XML.gxFromString

fromXText :: XText -> String
fromXText = XML.gxToString


type Node = XML.Node String XText


parseXML :: B.ByteString -> Either String LLSD
parseXML s = do
  case XML.parseTree' Nothing s of
    Right (XML.Element "llsd" _ cs) -> convertLLSD cs
    Right (_)                       -> fail "root element not llsd"
    Left err                        -> (fail . show) err


convertLLSD :: [Node] -> Either String LLSD
convertLLSD ns
    | exactlyOne filtns = convertValue (head filtns)
    | otherwise         = fail "not just one element under llsd"
    where filtns = dropWhitespaceText ns
          exactlyOne [_] = True
          exactlyOne _   = False

convertValue :: Node -> Either String LLSD
convertValue (XML.Element name _ ns) =
    case name of
        "undef"     -> convertUndef ns
        "boolean"   -> textOnly ns >>= convertBool
        "integer"   -> textOnly ns >>= convertInt
        "real"      -> textOnly ns >>= convertReal
        "string"    -> textOnly ns >>= convertString
        "uuid"      -> textOnly ns >>= convertUUID
        "date"      -> textOnly ns >>= convertDate
        "uri"       -> textOnly ns >>= convertURI
        "binary"    -> textOnly ns >>= convertBinary
        "map"       -> convertMap ns
        "array"     -> convertArray ns
        _           -> fail ("unknown value element: " ++ name)
convertValue (XML.Text text) = fail ("Unexpected text: " ++ fromXText text)

convertUndef :: [Node] -> Either String LLSD
convertUndef ns = case dropWhitespaceText ns of
    [] -> return undef
    _  -> fail "undef element isn't empty"
convertBool, convertInt, convertReal, convertString, convertUUID,
    convertDate, convertURI, convertBinary :: String -> Either String LLSD
convertBool t   = fmap toLLSD $ textBoolean t
convertInt t    = return $ toLLSD (fromLLSD (toLLSD t) :: Int)
convertReal t   = return $ toLLSD (fromLLSD (toLLSD t) :: Double)
convertString t = return $ toLLSD t
convertUUID t   = return $ toLLSD (fromLLSD (toLLSD t) :: UUID)
convertDate t   = return $ toLLSD (fromLLSD (toLLSD t) :: UTCTime)
convertURI t    = return $ toLLSD (fromLLSD (toLLSD t) :: URI)
convertBinary t =
    maybe (fail "invalid base64") (return . toLLSD . L.pack) $ Base64.decode t
    -- FIXME: should check that the encoding attribute is "base64"

convertArray :: [Node] -> Either String LLSD
convertArray = fmap toLLSD . mapM convertValue . dropWhitespaceText

convertMap :: [Node] -> Either String LLSD
convertMap ns = (toLLSD . Map.fromAscList) `fmap`
                (mapM convertAssoc =<< pairUp (dropWhitespaceText ns))
    where pairUp (a:b:rest) = do { ps <- pairUp rest; return $ (a,b) : ps }
          pairUp (_:[])     = fail "extra element in map"
          pairUp _          = return []

convertAssoc :: (Node, Node) -> Either String (String, LLSD)
convertAssoc (nk, nv) = (,) `fmap` convertKey nk `ap` convertValue nv

convertKey :: Node -> Either String String
convertKey (XML.Element "key" _ ns) = textOnly ns
convertKey (XML.Element name _ _) = fail ("expected key, found element: " ++ name)
convertKey (XML.Text text) = fail ("expected key, found text: " ++ fromXText text)


dropWhitespaceText :: [Node] -> [Node]
dropWhitespaceText ns = filter isNotWhitespace ns
    where isNotWhitespace (XML.Element _ _ _) = True
          isNotWhitespace (XML.Text t)        = not $ all isSpace $ fromXText t

textOnly :: [Node] -> Either String String
textOnly ns = (foldl' (\a b -> a ++ fromXText b) "") `fmap` textOnly' ns
    where textOnly' :: [Node] -> Either String [B.ByteString]
          textOnly' [] = return []
          textOnly' ((XML.Text t):ns') = textOnly' ns' >>= return . (t:)
          textOnly' ((XML.Element name _ _):_) =
            fail ("expected only text, found element: " ++ name)






formatXML :: LLSD -> L.ByteString
formatXML v = XML.formatNode $ XML.Element "llsd" [] [asNode v]
    -- FIXME: ? using XML.formatTree outputs an XML header

asNode :: LLSD -> Node
asNode LUndef         = XML.Element "undef"   [] []
asNode   (LBool b)    = XML.Element "boolean" [] [XML.Text $ booleanText b]
asNode v@(LInt _)     = XML.Element "integer" [] (textOf v)
asNode v@(LReal _)    = XML.Element "real"    [] (textOf v)
asNode v@(LString _)  = XML.Element "string"  [] (textOf v)
asNode v@(LUUID _)    = XML.Element "uuid"    [] (textOf v)
asNode v@(LDate _)    = XML.Element "date"    [] (textOf v)
asNode v@(LURI _)     = XML.Element "uri"     [] (textOf v)
asNode   (LBinary b)  = XML.Element "binary"  [("encoding", toXText "base64")] $
                            base64Text b
asNode (LArray vs)    = XML.Element "array"   [] $
                            map asNode $ expandLLSDArray vs
asNode (LMap vs)      = XML.Element "map"     [] $
                            concatMap asAssocNodes $ Map.assocs vs
    where asAssocNodes (k, v) = [XML.Element "key" [] [XML.Text $ toXText k], asNode v]

booleanText :: Bool -> B.ByteString
booleanText True = toXText "true"
booleanText False = toXText "false"
textBoolean :: String -> Either String Bool
textBoolean "true" = return True
textBoolean "false" = return False
textBoolean t = fail $ "invalid value for boolean: " ++ t
    -- FIXME: The spec implies that the value for the boolean node should be
    -- the string conversion, but that would make False be the empty string.
    -- The C++ implementation generates the word "false". Who's right?

textOf :: LLSD -> [Node]
textOf v = case t of
            "" -> [];
            _  -> [XML.Text $ toXText t]
     where t = fromLLSD v :: String

base64Text :: L.ByteString -> [Node]
base64Text b = if L.null b
                    then []
                    else [XML.Text $ toXText $ Base64.encode $ L.unpack b]


