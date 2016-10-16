{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Extended module Data.Either, from base, with additional
--               functions.
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  GHC specific language extensions.
--
-- Extended module "Data.Either", from base, with additional functions.
module Data.Either.Lifted
    ( module Data.Either

    , fromLeft
    , fromLeft_
    , fromRight
    , fromRight_

    , fromLeftA
    , fromLeftA_
    , fromRightA
    , fromRightA_

    , whenLeft
    , whenRight
    , onLeft
    , onRight

    , guardLeft
    , guardLeft_
    , guardRight
    , guardRight_
    )
  where

import Control.Applicative (Alternative, Applicative, pure)
import Control.Monad (guard)
import Data.Bool (Bool(False))
import Data.Function ((.), const, id)
import Data.Either


-- | Specialised 'either':
--
-- @
-- 'fromLeft' = 'either' 'id'
-- @
fromLeft :: (b -> a) -> Either a b -> a
fromLeft = either id
{-# INLINE fromLeft #-}

-- | Variant of 'fromLeft' that forgets the 'Right' value:
--
-- @
-- 'fromLeft_' def = 'fromLeft' ('const' def)
-- @
fromLeft_ :: a -> Either a b -> a
fromLeft_ = fromLeft . const
{-# INLINE fromLeft_ #-}

-- | Variant of 'fromLeft' where result is lifted to 'Applicative'. Defined as:
--
-- @
-- 'fromLeftA' = 'either' 'pure'
-- @
fromLeftA :: Applicative f => (b -> f a) -> Either a b -> f a
fromLeftA = either pure
{-# INLINE fromLeftA #-}

-- | Variant of 'fromLeftA' that forgets the left value. It's defined as:
--
-- @
-- 'fromLeftA_' def = 'fromLeftA' ('const' def)
-- @
fromLeftA_ :: Applicative f => f a -> Either a b -> f a
fromLeftA_ = fromLeftA . const
{-# INLINE fromLeftA_ #-}

-- | Specialised 'either':
--
-- @
-- 'fromRight' f = 'either' f 'id'
-- @
fromRight :: (a -> b) -> Either a b -> b
fromRight f = either f id
{-# INLINE fromRight #-}

-- | Variant of 'fromRight' that forgets the 'Left' value:
--
-- @
-- 'fromRight_' def = 'fromRight' ('const' def)
-- @
fromRight_ :: b -> Either a b -> b
fromRight_ = fromRight . const
{-# INLINE fromRight_ #-}

-- | Variant of 'fromRight' where result is lifted to 'Applicative'. Defined
-- as:
--
-- @
-- 'fromRightA' f = 'either' f 'pure'
-- @
--
-- === __Examples__
--
-- @
-- import Control.Monad.Except (MonadError, throwError)
--     -- From <https://hackage.haskell.org/package/mtl mtl> package.
--
-- example :: MonadError e m => 'Either' e a -> m a
-- example = 'fromRightA' throwError
-- @
--
-- @
-- import "Control.Exception" ('Control.Exception.throwIO')
--
-- example
--     :: 'Control.Exception.Exception' e
--     => (a -> e)
--     -> 'Either' a b
--     -> 'System.IO.IO' b
-- example f = 'fromRightA' ('Control.Exception.throwIO' . f)
-- @
fromRightA :: Applicative f => (a -> f b) -> Either a b -> f b
fromRightA f = either f pure
{-# INLINE fromRightA #-}

-- | Variant of 'fromRightA' that forgets the 'Left' value. It's defined as:
--
-- @
-- 'fromRightA_' def = 'fromRightA' ('const' def)
-- @
--
-- === __Examples__
--
-- @
-- 'fromRightA_' 'Data.Maybe.Nothing'
--     :: 'Either' a b -> 'Data.Maybe.Maybe' b
-- @
--
-- @
-- 'fromRightA_' ('Control.Exception.throwIO' MissingSomeMandatoryValue)
--     :: 'Either' a b -> 'System.IO.IO' b
-- @
fromRightA_ :: Applicative f => f b -> Either a b -> f b
fromRightA_ = fromRightA . const
{-# INLINE fromRightA_ #-}

-- | Short-hand for @\\x -> 'Control.Monad.when' ('isLeft' x)@.
whenLeft :: Applicative f => Either a b -> f () -> f ()
whenLeft (Left _) x = x
whenLeft _        _ = pure ()

-- | Short-hand for @\\x -> 'Control.Monad.when' ('isRight' x)@.
whenRight :: Applicative f => Either a b -> f () -> f ()
whenRight (Left _) _ = pure ()
whenRight _        x = x

-- | Perform action with 'Left' value.
--
-- === __Example__
--
-- @
-- example = do
--     r <- someActionReturningEither
--     'onLeft' r $ \\e -> 'Control.Monad.when' (isError e)
--         $ 'Control.Exception.throwIO' (errorToException e)
--     -- -->8--
-- @
--
-- @
-- example = do
--     r <- someActionReturningEither
--     'onLeft' r logMessage
--     -- -->8--
-- @
onLeft :: Applicative f => Either a b -> (a -> f ()) -> f ()
onLeft (Left a) f = f a
onLeft _        _ = pure ()

-- | Perform action with 'Right' value.
onRight :: Applicative f => Either a b -> (b -> f ()) -> f ()
onRight (Right b) f = f b
onRight _         _ = pure ()

-- | Guard that checks the 'Left' value.
guardLeft :: Alternative f => (a -> Bool) -> Either a b -> f ()
guardLeft p = guard . \case
    Left a -> p a
    Right _ -> False

-- | Guard that checks the 'Right' value.
guardRight :: Alternative f => (b -> Bool) -> Either a b -> f ()
guardRight p = guard . \case
    Left _ -> False
    Right b -> p b

-- | Short-hand for @'guard' . 'isLeft'@
guardLeft_ :: Alternative f => Either a b -> f ()
guardLeft_ = guard . isLeft

-- | Short-hand for @'guard' . 'isRight'@
guardRight_ :: Alternative f => Either a b -> f ()
guardRight_ = guard . isRight
