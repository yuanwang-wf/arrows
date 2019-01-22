# Learn Arrows

## Reading Notes John Hughes's [Programming With Arrows](http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf)


### Motivation

We can compose functions using `.` to build point-free function. However, we cannot use `.` to compose plain function with monadic function.

```haskell
count :: String -> String -> Int
count w = length . filter (== w) . words
```

```haskell
readFile :: String -> IO String
print :: Show a => a -> IO ()
```

```haskell
count w = print . length . filter (==w) . words . readFile --won't compile
```

Of course, we can treat `IO` as functor or Monad to compose them

```
countM w = fmap (length  . filter (== w) . words) . readFile
```
Maybe `countM` worth a little bit explantation, `readFile`'s type is `String -> IO String`, we use `fmap` lift `String -> Int` with `IO String` to `String -> IO Int`



Arrow represents a process

`arr` function lift a a function into an Arrow
`>>>` composes arrows


Arrows as computation

`&&&` is about to split the same input and feed to two process, and join the outputs
`***` joins two independent processes

Conditional compose arrows

```haskell
ifte :: Arrow arr => arr a Bool -> arr a b -> arr a b -> arr a b
ifte p f g = p &&& arr id >>> f ||| g
```

fixity of operations is missing

```haskell
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 >>>
```

Therefor, `ifte p f g = p &&& arr id >>> f ||| g` should be read as `ifte p f g = (p &&& arr id) >>> (f ||| g)`

`(|||) :: arr a c -> arr b c -> arr (Either a b) c`

`p &&& arr id`'s type should be `arr a (Bool, a)`, and `f ||| g` should be `arr (Either a a) b`

to make type checking, we need a function `(Bool , a) -> Either a a`

```haskell
ifte :: ArrowChoice arr => arr a Bool -> arr a b -> arr a b -> arr a b
ifte p f g = p &&& arr id >>> arr h >>> f ||| g
    where h :: (Bool, a) -> Either a a
          h (True, a) = Left a
          h (False, a) = Right a
```

mapA

```haskell
map _ []     = []
map f (x:xs) = f x : map f xs
```

```haskell
mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr listcase >>>
       arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))
    where listcase [] = Left ()
          listcase (x: xs) = Right (x, xs)
```

`arr listcase` type is `arr [a] Either () (a, [a])`, `arr (const [])` is `arr a [b]`
`f *** mapA f` type is `arr (a, [a]) (b, [b])`, `f *** mapA f >>> arr (uncurrat (:))` type is `arr (a, [a]) [b]`

`arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))` type is `arr (Either () (a, [a]) ) [b]`

How to implement `filterA`

we need `arr (a, [a]) (Either () a, [Either () a])`

```haskell
filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
filterA f = arr listcase >>>
            arr (const []) ||| ((f &&& arr id >>> arr h) *** filterA f >>> arr (uncurry (++)))
    where listcase [] = Left ()
          listcase (x: xs) = Right (x, xs)

          h :: (Bool , a) -> [a]
          h (False, _) = []
          h (True, a) = [a]
```


`data SP a b = Put b (SP a b) | Get (a -> SP a b)`

`first` operator builds a process that feeds the first components of its inputs through its argument process, while the second component bypass the argument process and recombined with its outputs. But what if the argument process does not produce one output per input ? Our solution is to buffer the unconsumed input until corresponding output are produced.




```haskell
instance Arrow SP where
    arr f = Get (\ a -> Put (f a) (arr f))
    sp1 >>> Put c sp2 = Put c (sp1 >>> sp2)
    Put b sp1 >>> Get f = sp1 >>> f b
    Get f >>> Get g = Get (\x -> f x >>> Get g)

    first = bypass []
        where bypass ds (Get f) = Get (\ (b, d -> bypass (ds ++ [d]) (f b))
              bypass (d: ds) (Put c sp) = Put (c, d) (bypass ds sp)
              bypass [] (Put c sp) = Get (\ (b, d) -> Put (c, d) (bypass [] sp))
```

Arrow Transformer ?

http://hackage.haskell.org/package/arrows-0.4.4.2/docs/Control-Arrow-Transformer-Stream.html#t:StreamArrow
