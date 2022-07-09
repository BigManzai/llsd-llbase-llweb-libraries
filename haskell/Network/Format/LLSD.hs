-- |
-- Module      :  Network.Format.LLSD
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable
--
-- An implementation of the LLSD data system.
--
-- LLSD is defined in draft-hamrick-vwrap-type-system-00. It is a structured 
-- data system used for interchange. See the draft for more details:
--  <http://tools.ietf.org/html/draft-hamrick-vwrap-type-system-00>

module Network.Format.LLSD (
        -- * Types
        LLSD,
        -- * Classes
        SData(..),
        SPath(..),
        -- * Functions
        -- ** Constants
        undef,
        -- ** Information
        size,
        -- ** Structure Accessors
        set, setAtIndex, setAtKey,
        get, getAtIndex, getAtKey,
        has, hasIndex,   hasKey,
        -- ** Path Based Accessor DSL
        llsd,
        (./),
        (.=),
        with,
        at,
        -- ** Conversion
        toMap,
        toList,
        prettyLLSD,
        parseXML,
        formatXML,
        parseBinary,
        formatBinary,
    )
    where

import Network.Format.LLSD.Binary
import Network.Format.LLSD.Internal
import Network.Format.LLSD.Pretty
import Network.Format.LLSD.XML
