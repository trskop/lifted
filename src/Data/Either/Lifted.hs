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
    , fromLeftA
    , fromRight
    , fromRightA
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
import Data.Function ((.), id)
import Data.Either


fromLeft :: (b -> a) -> Either a b -> a
fromLeft = either id

fromLeftA :: Applicative f => (b -> f a) -> Either a b -> f a
fromLeftA = either pure

fromRight :: (a -> b) -> Either a b -> b
fromRight f = either f id

fromRightA :: Applicative f => (a -> f b) -> Either a b -> f b
fromRightA f = either f pure

whenLeft :: Applicative f => Either a b -> f () -> f ()
whenLeft (Left _) x = x
whenLeft _        _ = pure ()

whenRight :: Applicative f => Either a b -> f () -> f ()
whenRight (Left _) _ = pure ()
whenRight _        x = x

onLeft :: Applicative f => Either a b -> (a -> f ()) -> f ()
onLeft (Left a) f = f a
onLeft _        _ = pure ()

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
