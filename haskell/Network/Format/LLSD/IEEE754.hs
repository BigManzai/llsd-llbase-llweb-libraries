{-# LANGUAGE MagicHash #-}

-- |
-- Module      :  Network.Format.LLSD.IEEE754
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  GHC specific

module Network.Format.LLSD.IEEE754 (
    encodeIEEEDouble,       -- :: Double -> Word64
    decodeIEEEDouble,       -- :: Word64 -> Double
    )
    where

import GHC.Prim
import GHC.Types
import GHC.Word

encodeIEEEDouble :: Double -> Word64
encodeIEEEDouble (D# x) = W64# (unsafeCoerce# x)

decodeIEEEDouble :: Word64 -> Double
decodeIEEEDouble (W64# x) = D# (unsafeCoerce# x)
