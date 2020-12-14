# PairDict
Lookup value-pairs from the left or the right.

## 👍 How to `PairDict`

## Example: cased letters
```elm
lowerUppercaseLetters=
  empty
  |>insert { left= 'a', right= 'A' }
  |>insert { left= 'b', right= 'B' }
  |>insert { left= 'c', right= 'C' }

upperCase char=
  rightOf char lowerUppercaseLetters
```
try in the [ellie of the brackets example](https://ellie-app.com/bNFGFYtdbDBa1)

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
please use a dict where it is more appropriate: `Dict`s are for one-way access

## Example: translation, synonymes...
```elm
englishGerman=
  fromList
    [ ( "elm", "Ulme" )
    , ( "git", "Schwachkopf" )
    ]
```
A right → left and backwards relationship isn't fitting,
as left or right can have multiple translations!

Please take a look at [elm-bidict](https://github.com/Janiczek/elm-bidict)
