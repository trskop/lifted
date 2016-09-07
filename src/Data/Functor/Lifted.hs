{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Extended module Data.Functor with additional
--               functions.
-- Copyright:    (c) 2011, 2013-2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  GHC specific language extensions.
--
-- Extended module "Data.Functor" with additional functions.
module Data.Functor.Lifted
    (
    -- * Data.Functor
    --
    -- | This module behaves as an extension to "Data.Functor" module, so we
    -- need to reexport it.
      module Data.Functor

    -- * Flipped Functor Application
    --
    -- | Two operators are provided for purpose of flipped 'Functor'
    -- application. One is '<&>', and the other one is '<$$>'. The difference
    -- being their fixity.
    , (<&>)
    , (<$$>)

    -- * Special Functor Applications
    , (<&$>)
    , (<#>)
    , (<##>)

#if !MIN_VERSION_base(4,7,0)
    -- * Compatibility with base >=4.7
    , ($>)
#endif
    )
  where

import Data.Function (($), flip)
import Data.Functor


-- | An infix synonym for fmap with flipped arguments.
--
-- The name of this operator is an allusion to 'Data.Function.&'. Note the
-- similarities between their types:
--
-- @
-- ('Data.Function.&')   ::              (a -> b) ->   a ->   b
-- ('<&>') :: 'Functor' f => (a -> b) -> f a -> f b
-- @
--
-- Whereas 'Data.Function.&' is function application, '<&>' is function
-- application lifted over a Functor.
--
-- This operator can be also found in
-- <https://hackage.haskell.org/package/lens lens> library.
--
-- ==== __Examples__
--
-- Convert from a Maybe Int to a Maybe String using show:
--
-- >>> Nothing <&> show
-- Nothing
-- >>> Just 3 <&> show
-- Just "3"
--
-- Convert from an Either Int Int to an Either Int String using show:
--
-- >>> Left 17 <&> show
-- Left 17
-- >>> Right 17 <&> show
-- Right "17"
--
-- Double each element of a list:
--
-- >>> [1, 2, 3] <&> (*2)
-- [2, 4, 6]
--
-- Apply even to the second element of a pair:
--
-- >>> (2, 2) <&> even
-- (2, True)
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
{-# INLINE (<&>) #-}

-- | An infix synonym for fmap with flipped arguments.
--
-- The name of this operator is an allusion to a flipped 'Data.Function.$'.
-- Note the similarities between their types:
--
-- @
-- 'flip' ('$') ::              (a -> b) ->   a ->   b
-- ('<$$>')   :: 'Functor' f => (a -> b) -> f a -> f b
-- @
--
-- Whereas '$' is function application, '<$$>' is function application lifted
-- over a Functor.
--
-- ==== __Simple Examples__
--
-- Convert from a Maybe Int to a Maybe String using show:
--
-- >>> Nothing <$$> show
-- Nothing
-- >>> Just 3 <$$> show
-- Just "3"
--
-- Convert from an Either Int Int to an Either Int String using show:
--
-- >>> Left 17 <$$> show
-- Left 17
-- >>> Right 17 <$$> show
-- Right "17"
--
-- Double each element of a list:
--
-- >>> [1, 2, 3] <$$> (*2)
-- [2, 4, 6]
--
-- Apply even to the second element of a pair:
--
-- >>> (2, 2) <$$> even
-- (2, True)
--
-- ==== __Examples__
--
-- We can use '<$$>' in cases when the pure operation, which is being 'fmap'ped,
-- is more complex than the function operating over a 'Functor'.
--
-- @
-- data Book = Book
--     { _title      :: 'Data.String.String'
--     , _author     :: 'Data.String.String'
--     , _characters :: ['Data.String.String']
--     }
--
-- -- Type signature could be also written as:
-- -- title :: Lens Book Book 'Data.String.String' 'Data.String.String'
-- title
--    :: Functor f
--    => ('Data.String.String' -> f 'Data.String.String')
--    -> Book -> f Book
-- title f s@Book{_title = a} = f a '<$$>' \b -> s{_title = b}
-- @
--
-- In contrast, if we use '<$>' then the above definition would be:
--
-- @
-- title f s@Book{_title = a} = (\b -> s{_title = b}) <$> f a
-- @
--
-- That is not a huge difference, due to the simplicity of the example, but for
-- more complicated (pure) functions we would have to define a helper function,
-- otherwise the readability would suffer. With '<$$>' we can define the more
-- complex (lambda) function inline without such issue.
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap
infixl 4 <$$>
{-# INLINE (<$$>) #-}

-- | Version of '<$>' with the same fixity as '<&>' function has.
(<&$>) :: Functor f => (a -> b) -> f a -> f b
(<&$>) = fmap
infixl 1 <&$>
{-# INLINE (<&$>) #-}

-- | Apply value to a function inside a functor. Similar to using:
--
-- @
-- \\f x -> f 'Control.Applicative.<*>' 'Control.Applicative.pure' x
-- @
--
-- But it does require only 'Functor' and not 'Applicative' constraint.
--
-- ==== __Examples__
--
-- >>> Just (+1) <#> 2
-- Just 3
-- >>> [(+1), (*2)] <#> 3
-- [4, 6]
--
-- For @instance 'Functor' ((->) r)@ this function behaves as infix version of
-- 'flip':
--
-- >>> (-) <#> 1 $ 2
-- 1
--
-- ==== __Implementation__
--
-- @
-- f '<#>' x = ('$' x) '<$>' f
-- @
(<#>) :: Functor f => f (a -> b) -> a -> f b
f <#> x = ($ x) `fmap` f
infixl 4 <#>
{-# INLINE (<#>) #-}

-- | Apply value to a function inside a functor. Similar to using:
--
-- @
-- \\x f -> f 'Control.Applicative.<*>' 'Control.Applicative.pure' x
-- @
--
-- But it does require only 'Functor' and not 'Control.Applicative.Applicative'
-- constraint. Flipped version of '<#>'.
--
-- ==== __Examples__
--
-- >>> Just (+1) <#> 2
-- Just 3
-- >>> [(+1), (*2)] <#> 3
-- [4, 6]
--
-- For @instance 'Functor' ((->) r)@ this function behaves as infix version of
-- 'flip':
--
-- >>> (-) <#> 1 $ 2
-- 1
--
-- ==== __Implementation__
--
-- @
-- x '<##>' f = ('$' x) '<$>' f
-- @
(<##>) :: Functor f => a -> f (a -> b) -> f b
x <##> f = ($ x) `fmap` f
infixl 4 <##>
{-# INLINE (<##>) #-}

-- {{{ Compatibility ----------------------------------------------------------

-- Function ($>) is available in base since version 4.7.0.0.
#if !MIN_VERSION_base(4,7,0)
-- | Flipped version of '<$'.
--
-- /Available in base >=4.7.0.0./
--
-- ==== __Examples__
--
-- Replace the contents of a @'Data.Maybe.Maybe' 'Data.Int.Int'@ with a
-- constant 'Data.String.String':
--
-- >>> Nothing $> "foo"
-- Nothing
-- >>> Just 90210 $> "foo"
-- Just "foo"
--
-- Replace the contents of an
-- @'Data.Either.Either' 'Data.Int.Int' 'Data.Int.Int'@ with a constant
-- 'Data.String.String', resulting in an @'Either' 'Int' 'Data.String.String'@:
--
-- >>> Left 8675309 $> "foo"
-- Left 8675309
-- >>> Right 8675309 $> "foo"
-- Right "foo"
--
-- Replace each element of a list with a constant 'SData.String.tring':
--
-- >>> [1, 2, 3] $> "foo"
-- ["foo", "foo", "foo"]
--
-- Replace the second element of a pair with a constant 'Data.String.String':
--
-- >>> (1, 2) $> "foo"
-- (1, "foo")
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
infixl 4 $>
{-# INLINE ($>) #-}
#endif

-- }}} Compatibility ----------------------------------------------------------
