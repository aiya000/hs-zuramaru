# ((((zuramaru)))) [![Build Status](https://travis-ci.org/aiya000/hs-zuramaru.svg?branch=master)](https://travis-ci.org/aiya000/hs-zuramaru) [![Hackage](https://img.shields.io/badge/Hackage-available-blue.svg)](https://hackage.haskell.org/package/zuramaru)

おらは 国木田花丸 ずら〜

A lisp dialect, an inline-lisp library

- [Hackage - A lisp processor, An inline-lisp, in Haskell](https://hackage.haskell.org/package/zuramaru)


# An inline-lisp in Haskell

`QuasiQuoter`s supports in this module

- [`Maru.QQ`](https://github.com/aiya000/hs-zuramaru/blob/master/src/Maru/QQ.hs)

## :muscle: Example :muscle:
### :one: S expression **parser**

As expressions

```haskell
>>> print [parse|123|]
AtomInt 123

>>> print [parse|sugar|]
AtomSymbol "sugar"

>>> print [parse|(1 2 3)|]
Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil))
```

As patterns

```haskell
>>> case AtomInt 123 of; [parse|123|] -> "good"
"good"

>>> case AtomInt 000 of; [parse|123|] -> "bad"; AtomInt _ -> "good"
"good"

>>> case Cons (AtomSymbol "x") (Cons (AtomInt 10) Nil) of; [parse|(x 10)|] -> "good"
"good"
```

As types (compile time calculations)

```haskell
>>> fromSing (sing :: Sing [parse|10|])
AtomInt 10

>>> fromSing (sing :: Sing [parse|konoko|])
AtomSymbol "konoko"

>>> fromSing (sing :: Sing [parse|(1 2 3)|])
Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil))
```

### :two: S expression parser + **preprocessor**

```haskell
>>> [parsePreprocess|sugar|]
AtomSymbol "sugar"

>>> [parsePreprocess|'10|]
Quote (AtomInt 10)

>>> [parsePreprocess|(quote 10)|]
Quote (AtomInt 10)
```

### :three: S expression parser + preprocessor + **evaluator**

```haskell
>>> [zurae|10|]
10

>>> [zurae|(print 10)|]
10Nil

>>> [zurae|'10|]
AtomInt 10

>>> [zurae|sugar|]
<interactive>:12:8: error:
    • Maru.QQ.zurae: an error is occured in the compile time: EvalException: "A symbol 'sugar' is not found"
    • In the quasi-quotation: [zurae|sugar|]
```

- - -

Please see below modules's doctests for more infomation

- [`Maru.QQ.ShortName`](https://github.com/aiya000/hs-zuramaru/blob/master/src/Maru/QQ/ShortName.hs)
- [`Maru.QQ`](https://github.com/aiya000/hs-zuramaru/blob/master/src/Maru/QQ.hs)
