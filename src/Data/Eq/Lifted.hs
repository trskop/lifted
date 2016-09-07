{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Extended module Data.Function, from base, with additional
--               functions.
-- Copyright:    (c) 2014-2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  GHC specific language extensions.
--
-- Extended module "Data.Function", from base, with additional functions.
module Data.Eq.Lifted
    (
    -- * Data.Bool
    --
    -- | This module behaves as an extension to "Data.Eq" module, so we need to
    -- reexport it.
      module Data.Eq

    -- * Lifted Eq Operations
    , (<==>)
    , (</=>)

    -- * Prefix Aliases For Eq Operations
    , is
    , isNot
    )
  where

import Control.Applicative (Applicative, liftA2)
import Data.Bool (Bool)
import Data.Eq


-- | Lifted '==' 'Eq' operation over an 'Applicative'.
(<==>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(<==>) = liftA2 (==)
{-# INLINE (<==>) #-}

-- | Lifted '/=' 'Eq' operation over an 'Applicative'.
(</=>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(</=>) = liftA2 (/=)
{-# INLINE (</=>) #-}

-- | Prefix version of '=='.
is :: Eq a => a -> a -> Bool
is = (==)
{-# INLINE is #-}

-- | Prefix version of '/='.
isNot :: Eq a => a -> a -> Bool
isNot = (/=)
{-# INLINE isNot #-}
