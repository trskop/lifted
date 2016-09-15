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
    )
  where

import Control.Applicative (Applicative, pure)
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
