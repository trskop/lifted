# Functions Lifted over Functor or Applicative

<!--
[![Hackage](http://img.shields.io/hackage/v/lifted.svg)][Hackage: lifted]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/lifted.svg)](http://packdeps.haskellers.com/reverse/lifted)
-->
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

<!--
[![Build](https://travis-ci.org/trskop/lifted.svg)](https://travis-ci.org/trskop/lifted)
-->


## Description

Functions from base, including few that aren't, lifted over Functor or
Applicative.

These functions have proved useful to the author over the years. All of them
have, at some point, being used in a production code.


## Examples

### Conversion of Error Handling

Various forms of error handling are used in Haskell libraries, and
applications. Some lifted functions allow us to convert between them with ease.
While the syntactic overhead is bigger then using functions customized to such
tasks; these functions are much more generic and so the code can be adjusted to
different error handling strategies with minimal changes.

Convert `Either a b` to `Maybe b` where `Left _` is interpreted as `Nothing`:

```Haskell
fromRightA_ Nothing :: Either a b -> Maybe b
```

Convert the same `Either a b` to `IO b` and throw exception when `Left` is encountered:

```Haskell
fromRightA_ (throwIO MissingValueException) :: Either a b -> IO b

-- or when:
--   MissingValueException :: String -> MissingValueException
-- then:

fromRightA (throwIO . MissingValueException) :: Either String b -> IO b
```

Convert `Maybe a` in to `Either String a` where the `String` argument contains
error message in case when `Nothing` is encountered:

```Haskell
fromJustA (Left "Error message.") :: Maybe a -> Either String a
```

Throw an exception on `Nothing`:

```Haskell
fromJustA (throwIO MissingValueException) :: Maybe a -> IO a
```

### Predicates

Simplified predicates:

```Haskell
import Data.Bool.Lifted ((<||>))
import Data.Eq.Lifted (is)


isSquareBracket :: Char -> Bool
isSquareBracket = is '[' <||> is ']'    -- Same as: (== '[') <||> (== ']')
```

Effectful conditions:

```Haskell
import System.Directory (doesFileExist, executable, getPermissions)
import Data.Bool.Lifted ((<&&>))


execute :: FilePath -> IO ()
execute fp = do
    isExe <- doesFileExist fp <&&> isExecutable fp
    if isExe
        then do
            -- -->8--
        else do
            -- -->8--
  where
    isExecutable = fmap executable . getPermissions
```


## License

The BSD 3-Clause License, see [LICENSE][] file for details.


## Contributions

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail.



<!--
[Hackage: lifted]:
  http://hackage.haskell.org/package/lifted
  "lifted package on Hackage"
-->

[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"

[LICENSE]:
  https://github.com/trskop/lifted/blob/master/LICENSE
  "License of lifted package."

[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
