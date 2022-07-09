-- |
-- Module      :  Network.Format.LLSD.Pretty
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Pretty printing of LLSD data

module Network.Format.LLSD.Pretty (
    prettyLLSD,
    showsPrecLLSD,
    )
    where

import qualified Data.UUID as UUID
import Network.Format.LLSD.Internal
import qualified Network.URI as URI
import Text.PrettyPrint.HughesPJ

import qualified Data.Map as M


instance Show LLSD where
    --show = render . prettyLLSD
    showsPrec = showsPrecLLSD



-- | Layout an LLSD as a PrettyPrint 'Doc' value
prettyLLSD :: LLSD -> Doc
prettyLLSD v = text "LLSD" <+> prettyValue v

prettyValue :: LLSD -> Doc
prettyValue LUndef = text "undef"
prettyValue (LBool True) = text "true"
prettyValue (LBool False) = text "false"
prettyValue (LInt v) = int v
prettyValue (LReal v) = double v
prettyValue (LString v) = doubleQuotes . text $ v
prettyValue (LUUID v) = text $ UUID.toString v
prettyValue v@(LDate _) = text $ fromLLSD v
prettyValue (LURI v) = text $ URI.uriToString id v ""
prettyValue (LBinary _) = text  "<binary>" -- FIXME: do something pretty
prettyValue (LArray vs) =
        brackets $ cat $ punctuate (text ", ")
            (map (nest 4 . prettyValue) (expandLLSDArray vs))
prettyValue (LMap vmap) =
        braces $ cat $ punctuate (text ", ")
            (map (nest 4 . prettyAssoc) (M.assocs vmap))
    where
        prettyAssoc (k,v) = text k <> colon <+> prettyValue v



showsPrecLLSD :: Int -> LLSD -> ShowS
showsPrecLLSD d v = showParen (d > appPrec) $ showString "LLSD " . showPrecValue v
    where appPrec = 10

showPrecValue :: LLSD -> ShowS
showPrecValue  LUndef = showString "undef"
showPrecValue (LBool True) = showString "True"
showPrecValue (LBool False) = showString "False"
showPrecValue (LInt v) = shows v       -- this won't read back in, needs ::Int
showPrecValue (LReal v) = shows v      -- this won't read back in, needs ::Double
showPrecValue (LString v) = showString v
showPrecValue (LUUID v) = shows v
showPrecValue (LDate v) = shows v
showPrecValue (LURI v) = shows v
showPrecValue (LBinary v) = shows v
showPrecValue (LArray vs) = buildShowList "[" ", " "]" $ showPrecArray (expandLLSDArray vs)
    where showPrecArray = map showPrecValue
showPrecValue (LMap vmap) = buildShowList "{" ", " "}" $ showPrecMap (M.assocs vmap)
    where showPrecMap = map (\(k, v) -> showString k . showString ": " . showPrecValue v)

buildShowList :: String -> String -> String -> [ShowS] -> ShowS
buildShowList pre mid post ss = showString pre . interpose ss . showString post
    where interpose [] = showChar ' '
          interpose ss' = foldr1 (\a b -> a . showString mid . b) ss'
