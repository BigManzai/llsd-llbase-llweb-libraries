{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      :  Network.Format.LLSD.LLIDL
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable

module Network.Format.LLSD.LLIDL (
        LLIDL(..),
        match, valid, hasAdditional, hasDefaulted, incompatible,

        Suite(),
        request, response,
        
        Definition(..), -- for quasi quoter
        fromDefinitions,
        
        parseValue,
        parseSuite,
        
        llidlSuite,
        llidlValue,
    )
    where

import Control.Monad
import Data.Data
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Network.Format.LLSD.Conversion
import Network.Format.LLSD.Internal
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

import qualified Data.ByteString.Lazy as Byte
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import qualified Network.URI as URI

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote


data LLIDL = UndefLLIDL
           | BoolLLIDL | IntLLIDL | RealLLIDL | StringLLIDL
           | DateLLIDL | UriLLIDL | UuidLLIDL | BinaryLLIDL
           | SelectNameLLIDL String | SelectBoolLLIDL Bool | SelectIntLLIDL Int
           | ArrayLLIDL Bool [LLIDL]
           | MapLLIDL [(String, LLIDL)]
           | DictLLIDL LLIDL
           | VariantLLIDL String
           | UndefinedLLIDL
           | BoundLLIDL VarContext LLIDL
    deriving (Typeable, Data)



baseResolve :: LLIDL -> LLSD -> Fidelity
baseResolve i = resolve i M.empty

match :: LLIDL -> LLSD -> Bool
match i v = baseResolve i v >= Converted

valid :: LLIDL -> LLSD -> Bool
valid i v = baseResolve i v >= Mixed

hasAdditional :: LLIDL -> LLSD -> Bool
hasAdditional i v = let r = baseResolve i v in r == Additional || r == Mixed

hasDefaulted :: LLIDL -> LLSD -> Bool
hasDefaulted i v = let r = baseResolve i v in r == Defaulted || r == Mixed

incompatible :: LLIDL -> LLSD -> Bool
incompatible i v = baseResolve i v < Mixed


type VarContext = M.Map String [LLIDL]

bindContext :: VarContext -> LLIDL -> LLIDL
bindContext = BoundLLIDL



type ReqRsp = (LLIDL, LLIDL)
data Definition = Resource String LLIDL LLIDL | Variant String LLIDL
    deriving (Typeable, Data)

data Suite = S {
    resources :: M.Map String ReqRsp,
    variants :: VarContext
    }

fromDefinitions :: [Definition] -> Suite
fromDefinitions = foldl' merge (S M.empty M.empty)
    where merge (S rs vs) (Resource k req rsp) = S (M.insert k (req, rsp) rs) vs
          merge (S rs vs) (Variant k var) = S rs (M.insertWith (++) k [var] vs)
          
resource :: String -> Suite -> ReqRsp
resource r st = M.findWithDefault undefinedResource r (resources st)
    where undefinedResource = (UndefinedLLIDL, UndefinedLLIDL)

request :: String -> Suite -> LLIDL
request r st = bindContext (variants st) (fst $ resource r st)

response :: String -> Suite -> LLIDL
response r st = bindContext (variants st) (snd $ resource r st)


type Resolver = VarContext -> LLSD -> Fidelity

resolve :: LLIDL -> Resolver

resolve UndefLLIDL                = resolveTo Matched
resolve BoolLLIDL                 = resolveType (undefined :: Bool)
resolve IntLLIDL                  = resolveType (undefined :: Int)
resolve RealLLIDL                 = resolveType (undefined :: Double)
resolve StringLLIDL               = resolveType (undefined :: String)
resolve DateLLIDL                 = resolveType (undefined :: Time.UTCTime)
resolve UriLLIDL                  = resolveType (undefined :: URI.URI)
resolve UuidLLIDL                 = resolveType (undefined :: UUID.UUID)
resolve BinaryLLIDL               = resolveType (undefined :: Byte.ByteString)

resolve (SelectNameLLIDL v)       = resolveSelect v
resolve (SelectBoolLLIDL v)       = resolveSelect v
resolve (SelectIntLLIDL v)        = resolveSelect v

resolve (ArrayLLIDL repeating is) = resolveArray repeating is
resolve (MapLLIDL assocs)         = resolveMap assocs
resolve (DictLLIDL i)             = resolveDict i
resolve (VariantLLIDL v)          = resolveVariant v

resolve UndefinedLLIDL            = resolveTo Incompatible

resolve (BoundLLIDL c i)          = \_ l -> resolve i c l


resolveTo :: Fidelity -> Resolver
resolveTo f _ _ = f

resolveType :: (ConvertTo a) => a -> Resolver
resolveType t _ l = snd $ convertAs t
    where convertAs :: (ConvertTo a) => a -> Conversion a
          convertAs _ = conversionFromLLSD l
          
resolveSelect :: (Eq a, ConvertTo a) => a -> Resolver
resolveSelect v _ l = select (conversionFromLLSD l)
    where select c = if v == fst c then snd c else Incompatible

resolveArray :: Bool -> [LLIDL] -> Resolver
resolveArray repeating is = arrayMatch
    where arrayMatch _ (LUndef)      = Defaulted
          arrayMatch c v@(LArray _)  = resolveAll c is0 $ toList v
          arrayMatch _ _             = Incompatible

          is0 = if repeating then [] else is

          resolveAll c (j:js) (l:ls)   = resolve j c l     ~&~ resolveAll c js ls
          resolveAll c (j:js) []       = resolve j c undef ~&~ resolveAll c js []
          resolveAll c []     ls@(_:_) = if repeating
                                            then resolveAll c is ls
                                            else Additional
          resolveAll _ []     []       = Matched

resolveMap :: [(String, LLIDL)] -> Resolver
resolveMap assocs = mapMatch
    where mapMatch _ (LUndef) = Defaulted
          mapMatch c (LMap m) = resolveAll ~&~ hasAll
            where resolveAll = foldr (\a r -> resolveOne a ~&~ r) Matched assocs
                  resolveOne (k, l) = maybe Defaulted (resolve l c) $ M.lookup k m
                  hasAll = if M.keysSet m `S.isSubsetOf` idlkeys
                                then Matched
                                else Additional
          mapMatch _ _        = Incompatible
          idlkeys = S.fromList $ map fst assocs

resolveDict :: LLIDL -> Resolver
resolveDict i = dictMatch
    where dictMatch _ (LUndef) = Defaulted
          dictMatch c (LMap m) = M.fold (\l r -> resolve i c l ~&~ r) Matched m
          dictMatch _ _        = Incompatible

resolveVariant :: String -> Resolver
resolveVariant v = varMatch
    where varMatch c l = foldl' pRes Incompatible vars
                              where pRes r i = resolve i c l ~|~ r
                                    vars = M.findWithDefault [] v c
    


showAnyError :: Either ParseError a -> Either String a
showAnyError = either (Left . show) (Right)

parseValue :: String -> Either String LLIDL
parseValue =  showAnyError . parse (between s eof value) ""

parseSuite :: String -> Either String Suite
parseSuite =  showAnyError . parse (between s eof suite) ""



value :: Parser LLIDL
value = simpleType <|> arrayType <|> mapType <|> selector <|> variant

simpleType :: Parser LLIDL
simpleType =    typeSymbol "undef"  UndefLLIDL
            <|> typeSymbol "string" StringLLIDL
            <|> typeSymbol "bool"   BoolLLIDL
            <|> typeSymbol "int"    IntLLIDL
            <|> typeSymbol "real"   RealLLIDL
            <|> typeSymbol "date"   DateLLIDL
            <|> typeSymbol "uri"    UriLLIDL
            <|> typeSymbol "uuid"   UuidLLIDL
            <|> typeSymbol "binary" BinaryLLIDL
    where typeSymbol t i = symbol t >> return i


arrayType :: Parser LLIDL
arrayType =
    try (do { cymbol '['; is <- valueList; cymbol ']'; return $ ArrayLLIDL False is })
    <|> (do { cymbol '['; is <- valueList; symbol "..."; cymbol ']';  return $ ArrayLLIDL True is })

valueList :: Parser [LLIDL]
valueList = sepEndBy1 value (cymbol ',')


mapType :: Parser LLIDL
mapType =  try (simpleMapType) <|> try (dictMapType)

simpleMapType :: Parser LLIDL
simpleMapType = do
    cymbol '{'; is <- memberList; cymbol '}'
    let keys = map fst is
        dupes = keys \\ nub keys
    if null dupes
        then return $ MapLLIDL is
        else fail $ "duplicate map keys: " ++ intercalate ", " dupes

memberList :: Parser [(String, LLIDL)]
memberList = sepEndBy1 member (cymbol ',')

member :: Parser (String, LLIDL)
member = do { n <- name; cymbol ':'; v <- value; return (n, v) }

dictMapType :: Parser LLIDL
dictMapType = do
    cymbol '{'; cymbol '$'; cymbol ':'; i <- value; cymbol '}'
    return $ DictLLIDL i


selector :: Parser LLIDL
selector =
    try (lexeme $ between (char '"') (char '"') name' >>= return . SelectNameLLIDL)
    <|> (symbol "true" >> return (SelectBoolLLIDL True))
    <|> (symbol "false" >> return (SelectBoolLLIDL False))
    <|> ((lexeme $ integer) >>= return . SelectIntLLIDL)




variant :: Parser LLIDL
variant = char '&' >> name >>= return . VariantLLIDL


suite :: Parser Suite
suite = definitions >>= return . fromDefinitions

definitions :: Parser [Definition]
definitions = many ( variantDef <|> resourceDef )

variantDef :: Parser Definition
variantDef = return Variant `ap` between (char '&') (cymbol '=') name `ap` value

resourceDef :: Parser Definition
resourceDef = do
    res <- resName
    (req, resp) <- (postRes <|> getRes <|> getPutRes)
    return $ Resource res req resp
    

resName :: Parser String
resName = symbol "%%" >> name

postRes :: Parser (LLIDL, LLIDL)
postRes = do
    req  <- symbol "->" >> value
    resp <- symbol "<-" >> value
    return (req, resp)
    
getRes :: Parser (LLIDL, LLIDL)
getRes = do
    body <- symbol "<<" >> value
    return (UndefinedLLIDL, body)

getPutRes :: Parser (LLIDL, LLIDL)
getPutRes = do
    body <- (symbol "<>" <|> symbol "<x>") >> value
    return (body, body)



-- These follow the pattern in Parsec.Token, but we redo it here because the
-- specifics of whitepsace in LLIDL don't match the assumptions in Parsec.Token.

symbol :: String -> Parser ()
symbol t = try (lexeme $ string t >> return ())

cymbol :: Char -> Parser ()
cymbol c =  lexeme $ char c >> return ()

name :: Parser String
name = lexeme name'

lexeme :: Parser a -> Parser a
lexeme p = do { r <- p; s; return r }


-- Non-lexmeme parsers

s :: Parser ()
s = skipMany (tab <|> newline <|> space <|> comment)
    -- unfortunate name, but that is what it is called in the Internet Draft

comment :: Parser Char
comment = char ';' >> (skipMany $ noneOf "\n") >> newline >> return ';'
    --FIXME: newline in spec is \n, \r, or \r\n
    --FIXME: returning Char is ugly just to make s easier

name' :: Parser String
name' = do { c <- nameStart; cs <- many nameContinue; return (c:cs) }
    where nameStart = idStart <|> char '_'
          nameContinue = idContinue <|> char '_' <|> char '/'
          idStart = oneOf ['A'..'Z'] <|> oneOf ['a'..'z']
          idContinue = idStart <|> oneOf ['0'..'9']

integer :: Parser Int
integer = many1 digit >>= return . read


--
-- QuasiQuote Support
--

llidlSuite, llidlValue :: QuasiQuoter
llidlSuite = QuasiQuoter (qqToSuite $ qq definitions) failPat
llidlValue = QuasiQuoter (qq value)                   failPat

qq :: (Data t) => (Parser t) -> String -> TH.Q TH.Exp
qq pThing input = do
    loc <- TH.location
    let pos = newPos (TH.loc_filename loc)
                     (fst (TH.loc_start loc))
                     (snd (TH.loc_start loc))
    case parse (setPosition pos >> between s eof pThing) "" input of
        Left err  -> fail $ show err
        Right e   -> dataToExpQ (const Nothing) e

qqToSuite :: (String -> TH.Q TH.Exp) -> (String -> TH.Q TH.Exp)
qqToSuite f = \input -> do
    eDefs <- f input
    return $ TH.AppE (TH.VarE (TH.mkName "fromDefinitions")) eDefs

failPat :: String -> TH.Q TH.Pat
failPat _ = fail "can't pattern match llidl"
