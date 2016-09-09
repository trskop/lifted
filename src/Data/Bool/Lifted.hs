{-# LANGUAGE CPP #-}
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
module Data.Bool.Lifted
    (
    -- * Data.Bool
    --
    -- | This module behaves as an extension to "Data.Bool" module, so we need
    -- to reexport it.
      module Data.Bool

    -- * Lifted Bool Operations
    , (<||>)
    , (<&&>)
    , notA
    , boolA

    -- * Lifted Bool Values
    , falseA
    , trueA

#if !MIN_VERSION_base(4,7,0)
    -- * Compatibility with base >=4.7
    , bool
#endif
    )
  where

import Control.Applicative (Applicative, liftA2, pure)
import Data.Bool
import Data.Functor (Functor, fmap)


-- | Lifted '&&' 'Bool' operation over an 'Applicative'.
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
{-# INLINE (<||>) #-}

-- | Lifted '&&' 'Bool' operation over an 'Applicative'. It's name is somewhat
-- confusing due to the existence of '<&>' which is a lifted function
-- application. Please make sure to understand the distinction to avoid
-- confusion.
--
-- ==== __Examples__
--
-- Operator '<&&>' can simplify predicates:
--
-- @
-- isReserved :: 'Data.Char.Char' -> 'Bool'
-- isReserved = 'Data.Eq.Lifted.is' '[' '<||>' 'Data.Eq.Lifted.is' ']'
--   where
--     'Data.Eq.Lifted.is' = 'Data.Eq.=='   -- See "Data.Eq.Lifted" module.
-- @
--
-- Function '<&&>' can be used to combine stateful computations, even those
-- that involve IO:
--
-- @
-- import System.Directory (doesFileExist, executable, getPermissions)
--     -- From <https://hackage.haskell.org/package/directory directory> package.
--
--
-- execute :: 'System.IO.FilePath' -> 'System.IO.IO' ()
-- execute fp = do
--     isExe <- doesFileExist fp <&&> isExecutable fp
--     if isExe
--         then do
--             -- -->8--
--         else do
--             -- -->8--
--   where
--     isExecutable = 'fmap' executable . getPermissions
-- @
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
{-# INLINE (<&&>) #-}

-- | Function 'not' lifted over a 'Functor'.
notA :: Functor f => f Bool -> f Bool
notA = fmap not
{-# INLINE notA #-}

-- | Function 'bool' lifted over a 'Functor'.
boolA :: Functor f => a -> a -> f Bool -> f a
boolA x y = fmap (bool x y)
{-# INLINE boolA #-}

-- | 'False' lifted over an 'Applicative'.
--
-- For @instance 'Applicative' ((->) r)@ this behaves as
-- @'Data.Function.const' 'False'@.
falseA :: Applicative f => f Bool
falseA = pure False
{-# INLINE falseA #-}

-- | 'True' lifted over an 'Applicative'.
--
-- For @instance 'Applicative' ((->) r)@ this behaves as
-- @'Data.Function.const' 'True'@.
trueA :: Applicative f => f Bool
trueA = pure True
{-# INLINE trueA #-}

-- {{{ Compatibility ----------------------------------------------------------

-- Function 'bool' is available in base since version 4.7.0.0.
#if !MIN_VERSION_base(4,7,0)
-- | Case analysis for the 'Bool' type. @'bool' x y p@ evaluates to @x@
-- when @p@ is 'False', and evaluates to @y@ when @p@ is 'True'.
--
-- This is equivalent to @if p then y else x@; that is, one can
-- think of it as an if-then-else construct with its arguments
-- reordered.
--
-- /Available in base >=4.7.0.0./
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bool "foo" "bar" True
-- "bar"
-- >>> bool "foo" "bar" False
-- "foo"
--
-- Confirm that @'bool' x y p@ and @if p then y else x@ are
-- equivalent:
--
-- >>> let p = True; x = "bar"; y = "foo"
-- >>> bool x y p == if p then y else x
-- True
-- >>> let p = False
-- >>> bool x y p == if p then y else x
-- True
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
#endif

-- }}} Compatibility ----------------------------------------------------------
