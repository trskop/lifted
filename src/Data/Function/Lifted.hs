{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Extended module Data.Function, from base, with additional
--               functions.
-- Copyright:    (c) 2015-2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  GHC specific language extensions.
--
-- Extended module "Data.Function", from base, with additional functions.
module Data.Function.Lifted
    (
    -- * Data.Function
    --
    -- | This module behaves as an extension to "Data.Function" module, so we
    -- need to reexport it.
      module Data.Function

    -- * Extra Function Application Combinators
    , ($$)
    , (&$)

#if !MIN_VERSION_base(4,8,0)
    -- * Compatibility with base >=4.8
    , (&)
#endif
    )
  where

import Data.Function


-- | Flipped version of '$'.
($$) :: a -> (a -> b) -> b
x $$ f = f x
infixr 0 $$
{-# INLINE ($$) #-}

-- | Same as function '$', but with the same fixity as '&' function has.
(&$) :: (a -> b) -> a -> b
f &$ a = f a
infixl 1 &$
{-# INLINE (&$) #-}

-- {{{ Comptibility -----------------------------------------------------------

-- Function (&) is available in base since version 4.8.0.0. Haddock
-- comment taken from Data.Function module from base.
#if !MIN_VERSION_base(4,8,0)
-- | '&' is a reverse application operator. This provides notational
-- convenience. Its precedence is one higher than that of the forward
-- application operator '$', which allows '&' to be nested in '$'.
--
-- /Available in base >=4.7.0.0./
(&) :: a -> (a -> b) -> b
x & f = f x
infixl 1 &
{-# INLINE (&) #-}
#endif

-- }}} Comptibility -----------------------------------------------------------
