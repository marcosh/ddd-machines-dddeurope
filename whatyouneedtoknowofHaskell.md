# What you need to know of Haskell

## types

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

## functions



## data declaration

```haskell
data Foo a b c = MkFoo
  { foo1 :: a -> b
  , foo2 :: Int -> c -> String
  }
```