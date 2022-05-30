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

Concrete types always start with a capital letter, type variables always start with a lowercase character

```haskell
emptyList :: [a]
emptyList = []
```

This means that the empty list `[]` could be interpreted as a list of elements of any possible type `a`.

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

`Bar1` and `Bar2` are two separate constructors for `Bar`, of type `Int -> Bar` and `String -> Bar` respectively.

### pattern matching

To consume a value of type `Bar`, we can pattern match on the constructors and define the result case by case

```haskell
toString :: Bar -> String
toString (Bar1 i) = show i
toString (Bar2 s) = s
```

### newtypes

A `newtype` is an optimization for a data declaration with a single constructor containing a single field.
