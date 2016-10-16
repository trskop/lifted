{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Extended module Data.Maybe, from base, with additional
--               functions.
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  GHC specific language extensions.
--
-- Extended module "Data.Maybe", from base, with additional functions.
module Data.Maybe.Lifted
    ( module Data.Maybe
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


fromJustA :: Applicative f => f a -> Maybe a -> f a
fromJustA d = maybe d pure

whenJust :: Applicative f => Maybe a -> f () -> f ()
whenJust Nothing _ = pure ()
whenJust _       x = x

whenNothing :: Applicative f => Maybe a -> f () -> f ()
whenNothing Nothing x = x
whenNothing _       _ = pure ()

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
