# PairDict
Lookup value-pairs from the left or the right.

Let's compare


### normal `Dict`

> you want `🔑 1`?

        ( 🔑 0, 🌳 )
      → ( 🔑 1, 🍐 )
        ( 🔑 2, 🍐 )

> Going through while comparing your key... Ah! Here it is:

        🍐

### `PairDict`

> you want the left value of `🗝️ 1` and the right value of `🔑 0`?

      → ( 🔑 0, 🗝️ 2 )
        ( 🔑 2, 🗝️ 0 )
        ( 🔑 1, 🗝️ 1 ) ←

> Going through while checking, if your key is equal... Ah! Here they are:

        🔑 1 and 🗝️ 2


<br><br>

## 👍 How to `PairDict`

## Example: cased letters
```elm
lowerUppercaseLetters=
  empty
  |>insert ( 'a', 'A' )
  |>insert ( 'b', 'B' )
  |>insert ( 'c', 'C' )

upperCase char=
  rightOf char lowerUppercaseLetters
```
try in the [ellie for the example cased letters](https://ellie-app.com/bNNjrC6r2TWa1)

## Example: periodic table

```elm
type Element=
  Hydrogen
  | Helium

elementPair { element, atomicNumber )=
  ( element, protonCount )

elementAtomicNumberPairdict=
  empty
  |>insert ({ element= Hydrogen, atomicNumber= 1 } |>elementPair)
  |>insert ({ element= Helium, atomicNumber= 2 } |>elementPair)

atomicNumberByElement=
  toDict elementAtomicNumberPairdict
```

## Example: brackets
You have pairs that belong together:
```elm
brackets=
  fromList
    [ ( '(', ')' )
    , ( '{', '}' )
    ]
typeChar character=
  case leftOf character brackets of
    Just opening->
      String.fromList [ opening, character ]

    Nothing->
      case rightOf character brackets of
        Just closing->
          String.fromList [ character, closing ]

        Nothing->
          String.fromChar character

"Typing (: " ++(typeChar '(') ++". Even }: " ++(typeChar '}')
```
<br>

## 👎 How not to `PairDict`

## Example: automatic answers
```elm
answers=
  fromList
    [ ( "Hi", "Hi there!" )
    , ( "Bye", "Ok, have a nice day and spread some love.")
    , ( "How are you", "I don't have feelings :(" )
    , ( "Are you a robot", "I think the most human answer is 'Haha... yes'" )
    ]
```
please use a `Dict` where it is more appropriate: **`Dict`s are for one-way access**

## Example: translation, synonymes...
```elm
englishGerman=
  fromList
    [ ( "elm", "Ulme" )
    , ( "git", "Schwachkopf" )
    ]
```
A right → left and backwards relationship is only fitting,
when **left or right don't have multiple translations**.

Please take a look at [elm-bidict](https://github.com/Janiczek/elm-bidict)

## Example: partners, opposites...
```elm
partners=
  empty
  |>insert (Pair "Ann" "Alan")
  |>insert (Pair "Alex" "Alastair")
  |>insert (Pair "Alan" "Ann") --wait, this is no duplicate and gets inserted?
```
A `PairDict` ony makes sense, when the **left & right sides describe something different**.
