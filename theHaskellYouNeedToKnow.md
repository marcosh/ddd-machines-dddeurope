# What you need to know of Haskell

## declarations and types

When we declare a value `f`, we usually first write the type of `f` and then, on a separate line, its actual value. For example

```haskell
fortytwo :: Int
fortytwo = 42

hi :: String
hi = "hi"
```

### type variables

Concrete types always start with a capital letter, type variables (i.e. generics) always start with a lowercase character

```haskell
emptyList :: [a]
emptyList = []
```

This means that the empty list `[]` could be interpreted as a list of elements of any possible type `a`

## functions

Functions types are defined using the `->` symbol

```haskell
foo :: Int -> Int -> Int -> Int
foo i j k = i + j * k
```

This means that `foo` is a function with 3 `Int` arguments.

Functions are curried by default (e.g. `f 1` is a function `Int -> Int -> Int`)

The same function could be written in an anonymous form as

```haskell
\i j k -> i + j * k
```

### function application

Function application is denoted with whitespace.
For example, `foo 1 2 3` equals `1 + 2 * 3 = 7`

## data declaration

A data declaration looks as follows:

```haskell
data Foo a b c = MkFoo
  { foo1 :: a -> b
  , foo2 :: Int -> c -> String
  }
```

`Foo` is the name of the type, which has three type variables `a`, `b` and `c`.
`MkFoo` is the constructor, which is a function `(a -> b) -> (Int -> c -> String) -> Foo a b c`
`foo1` and `foo2` are fields which have respectively type `Foo a b c -> a -> b` and `Foo a b c -> Int -> c -> String`

### multiple constructors

A data declarations could have several constructors, as in

```haskell
data Bar
  = Bar1 Int
  | Bar2 String
```

`Bar1` and `Bar2` are two separate constructors for `Bar`, of type `Int -> Bar` and `String -> Bar` respectively

### pattern matching

To consume a value of type `Bar`, we can pattern match on the constructors and define the result case by case

```haskell
toString :: Bar -> String
toString (Bar1 i) = show i
toString (Bar2 s) = s
```

### newtypes

A `newtype` is an optimization for a data declaration with a single constructor containing a single field. The syntax is the same as a `data` declaration, using the `newtype` keyword instead

## IO

`IO` is the datatype which is used in Haskell to describe interactions with the external world. A value of type `IO a` describes an interaction with the external world which produces a value of type `a`

### map

If you have a value `x :: IO a` and a pure function `f :: a -> b`, it is not possible to apply directly `f` to `x`. Instead, we can map the function `f` over `x` and obtain a value `fmap f x = f <$> x` of type `IO b`

### pure

If we need a value of type `IO a`, and we only have a value `x` of type `a`, we can lift `x` to `IO` using the `pure` function, obtaining a `pure x` value of type `IO a`.
