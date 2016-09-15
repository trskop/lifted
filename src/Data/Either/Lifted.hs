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
    )
  where

import Control.Applicative (Applicative, pure)
import Data.Function (id)
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
whenLeft (Left _) _ = pure ()
whenLeft _        x = x

whenRight :: Applicative f => Either a b -> f () -> f ()
whenRight (Left _) x = x
whenRight _        _ = pure ()

onLeft :: Applicative f => Either a b -> (a -> f ()) -> f ()
onLeft (Left a) f = f a
onLeft _        _ = pure ()

onRight :: Applicative f => Either a b -> (b -> f ()) -> f ()
onRight (Right b) f = f b
onRight _         _ = pure ()
