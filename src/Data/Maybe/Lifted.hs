{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Extended module Data.Maybe, from base, with additional
--               functions.
-- Copyright:    (c) 2016-2017 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  GHC specific language extensions.
--
-- Extended module "Data.Maybe", from base, with additional functions.
module Data.Maybe.Lifted
    ( module Data.Maybe
    , maybeA
    , fromJustA
    , whenJust
    , whenNothing
    , onJust

    , guardNothing
    , guardJust
    , guardJust_
    )
  where

import Control.Applicative (Alternative, Applicative, pure)
import Control.Monad (guard)
import Data.Bool (Bool(False))
import Data.Function ((.))
import Data.Maybe


-- | @'maybeA' b f = 'maybe' ('pure' b) f@
maybeA :: Applicative f => b -> (a -> f b) -> f b
maybeA b = maybe (pure b)

-- | @'fromJustA' d = 'maybe' d 'pure'@
fromJustA :: Applicative f => f a -> Maybe a -> f a
fromJustA d = maybe d pure

-- | @'whenJust' x action = 'maybe' ('pure' ()) action x@
whenJust :: Applicative f => Maybe a -> f () -> f ()
whenJust Nothing _ = pure ()
whenJust _       x = x

-- | @'whenNothing' x action = 'maybe' action (\\_ -> 'pure' ()) x@
whenNothing :: Applicative f => Maybe a -> f () -> f ()
whenNothing Nothing x = x
whenNothing _       _ = pure ()

-- | Run action on a 'Just' value.
--
-- Here are few examples of functions to which 'onJust' is equivalent:
--
-- @
-- 'onJust' x f = 'maybe' ('pure' ()) f x
-- 'onJust' x f = 'maybeA' () f x
-- @
--
-- Usage example:
--
-- @
-- \\possiblyHandle -> do
--     onJust possiblyHandle $ setCorrectBuffering
--     let handle = 'fromMaybe' stdout possiblyHandle
--     -- -->8--
--   where
--     setCorrectBuffering = -- -->8--
-- @
onJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
onJust Nothing  _ = pure ()
onJust (Just a) f = f a

-- | Short-hand for @'guard' . 'isNothing'@
guardNothing :: Alternative f => Maybe a -> f ()
guardNothing = guard . isNothing

-- | Guard on a 'Just' vaule.
guardJust :: Alternative f => (a -> Bool) -> Maybe a -> f ()
guardJust p = guard . \case
    Nothing -> False
    Just a -> p a

-- | Short-hand for @'guard' . 'isJust'@
guardJust_ :: Alternative f => Maybe a -> f ()
guardJust_ = guard . isJust
