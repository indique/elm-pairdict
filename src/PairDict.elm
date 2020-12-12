module PairDict exposing
  ( PairDict
  , empty, fromDict, fromList
  , rightOf, leftOf, size
  , lefts, rights
  , Pair, map, insert
  , removeLeft, removeRight
  , foldr, foldl
  , union
  , swapLeftRight
  , toDict
  )
{-| 
@docs PairDict, Pair


## create
@docs empty, fromDict, fromList

## access
@docs rightOf, leftOf

## properties
@docs size

## in
@docs insert, union

## out
@docs removeLeft, removeRight

## transform
@docs lefts, rights, swapLeftRight, foldr, foldl, map, toDict
-}

--**remove me!** publish https://korban.net/posts/elm/2018-10-02-basic-steps-publish-package-elm-19/

import Dict exposing (Dict)

{-| Want to look up value-pairs from the left or the right?

## Example: cased letters
    casedLetters=
      empty
      |>insert { left= 'a', right= 'A' }
      |>insert { left= 'b', right= 'B' }
      |>insert { left= 'c', right= 'C' }

    upperCase char=
      rightOf char casedLetters
-}
type PairDict left right=
  PairDict
    { fromLeft: Dict left right
    , fromRight: Dict right left
    }

{-| start with an empty `PairDict`

    empty --fromList []
-}
empty: PairDict left right
empty=
  PairDict
    { fromLeft= Dict.empty
    , fromRight= Dict.empty
    }

{-| Create a `PairDict` conveniently from `Pair`-`Tuple`s. `left` is the first, `right` is the second.
If right or left values are given multiple times, the value **first** in the `List` is **prefered**.
    
    lowerToUpperLetters=
      Dict.empty
      |>Dict.insert 'a' 'A'
      |>Dict.insert 'b' 'B'

    lowerUpperLetters= fromDict lowerToUpperLetters
-}
fromDict:
  Dict comparableLeft comparableRight
  ->PairDict comparableLeft comparableRight
fromDict=
  Dict.foldl
    (\k v-> insert ({ left= k, right= v }))
    empty

{-| Create a `PairDict` conveniently from `Tuple`s. `left` is the first, `right` is the second.
If right or left values are given multiple times, the value **first** in the `List` is **prefered**.
    
    fromList
      [ ( 'b', 'B' ) --+ { left= 'b', right= 'B' }
      , ( 'a', 'A' ) --+ { left= 'a', right= 'A' }
      , ( 'b', 'C' ) --ignored, as the left value already exists
      , ( 'c', 'A' ) --ignored, as the right value already exists
      , ( 'c', 'C' ) --+ { left= 'c', right= 'C' }
      ]
-}
fromList:
  List ( comparableLeft, comparableRight )
  ->PairDict comparableLeft comparableRight
fromList=
  List.foldl
    (\( v0, v1 )-> insert { left= v0, right= v1 })
    empty


{-| `Just` the right value if the left value is present in the `PairDict`, else `Nothing`

    casedLetters=
      empty
      |>insert { left= 'a', right= 'A' }
      |>insert { left= 'b', right= 'B' }

    lowerCase char=
      Dict.leftOf char casedLetters
-}
rightOf:
  comparableLeft
  ->PairDict comparableLeft comparableRight
  ->Maybe comparableRight
rightOf left (PairDict pairDict)=
  Dict.get left pairDict.fromLeft

{-| `Just` the right value if the left value is present in the `PairDict`, else `Nothing`

    casedLetters=
      empty
      |>insert { left= 'a', right= 'A' }
      |>insert { left= 'b', right= 'B' }
      |>insert { left= 'c', right= 'C' }

    upperCase char=
      rightOf char casedLetters
-}
leftOf:
  comparableRight
  ->PairDict comparableLeft comparableRight
  ->Maybe comparableLeft
leftOf right (PairDict pairDict)=
  Dict.get right pairDict.fromRight

{-| How many pairs there are.

    size empty --0
    
    size
      (fromList
        (List.range 0 41 |>List.map (\i-> ( i, i )))
      )
    --42
-}
size: PairDict comparableLeft comparableRight ->Int
size (PairDict pairDict)=
  Dict.size pairDict.fromLeft

{-| values on the left

    brackets=
      fromList [ ( '(', ')' ), ( '{', '}' ) ]
    opening= lefts brackets
-}
lefts: PairDict left right ->List left
lefts (PairDict pairDict)=
  Dict.keys pairDict.fromLeft

{-| values on the right

    brackets=
      fromList [ ( '(', ')' ), ( '{', '}' ) ]
    closing= rights brackets
-}
rights: PairDict left right ->List right
rights (PairDict pairDict)=
  Dict.keys pairDict.fromRight


{-| `left` & `right` as a value-pair
-}
type alias Pair comparableLeft comparableRight=
  { left: comparableLeft
  , right: comparableRight
  }

{-| Put in a left-right-`Pair`
If either value is already present, the **dict is unchanged**.

    empty
    |>insert { left= "(", right= ")" }
    |>insert { right= "fire", left= "water" }
    --fromList
    --  [ ( "(", ")" ) ( "water", "fire" ) ]
-}
insert:
  Pair comparableLeft comparableRight
  ->PairDict comparableLeft comparableRight
  ->PairDict comparableLeft comparableRight
insert { left, right } (PairDict pairDict)=
  let { fromLeft, fromRight }= pairDict
  in
  (case
    (&&)
      ((==) (Dict.get left fromLeft) Nothing)
      ((==) (Dict.get right fromRight) Nothing)
    of
    True->
      { fromLeft= Dict.insert left right fromLeft
      , fromRight= Dict.insert right left fromRight
      }
    False->
      pairDict
  )
  |>PairDict


{-| Merge 2 `PairDict`s.
If a value on the left or right is present, prefer the second dict.

    numberOperatorNames=
      fromList
        [ ( '+', "plus" )
        , ( '-', "minus" )
        ]
    boolOperatorNames=
      fromList
        [ ( '∧', "and" )
        , ( '∨', "or" )
        ]
    operatorNames=
      union numberOperatorNames boolOperatorNames
-}
union:
  PairDict comparableLeft comparableRight
  ->PairDict comparableLeft comparableRight
  ->PairDict comparableLeft comparableRight
union added preferred=
  added
  |>foldl insert preferred

{-| Reduce the left-right `Pair`s from highest left to lowest left

    brackets=
      fromList
        [ ( '(', ')' )
        , ( '{', '}' )
        ]
    openingAndClosing=
      foldl
        (\{ left, right } acc->
          String.fromList [ left, right ]
        )
        []
      --[ "{}", "()" ]
-}
foldr:
  (Pair comparableLeft comparableRight
  ->result -> result
  )
  ->result
  ->PairDict comparableLeft comparableRight
  ->result
foldr reduce initial (PairDict pairDict)=
  pairDict.fromLeft
  |>Dict.foldr
      (\left right-> reduce (Pair left right))
      initial

{-| Reduce the left-right `Pair`s from lowest left to highest left

    brackets=
      fromList
        [ ( '(', ')' )
        , ( '{', '}' )
        ]
    openingAndClosing=
      brackets
      |>foldl
          (\{ left, right } acc->
            acc ++[ String.fromList [ left, right ] ]
          )
          []
      --[ "()", "{}" ]
-}
foldl:
  (Pair comparableLeft comparableRight
  ->result ->result
  )
  ->result
  ->PairDict comparableLeft comparableRight
  ->result
foldl reduce initial (PairDict pairDict)=
  pairDict.fromLeft
  |>Dict.foldl
      (\left right-> reduce (Pair left right))
      initial


{-| remove left-right pair.
If **`left` does not exist**, the `PairDict` is **unchanged**

    empty
    |>insert "(" ")"
    |>removeLeft "("
    --empty
-}
removeLeft:
  comparableLeft
  ->PairDict comparableLeft comparableRight
  ->PairDict comparableLeft comparableRight
removeLeft left pairDict=
  rightOf left pairDict
  |>Maybe.map
      (\right->
        remove { left= left, right= right } pairDict
      )
  |>Maybe.withDefault
      pairDict

{-| remove left-right pair.
If `right` does not exist, the `PairDict` is unchanged

    empty
    |>insert "(" ")"
    |>removeRight ")"
    --empty
-}
removeRight:
  comparableRight
  ->PairDict comparableLeft comparableRight
  ->PairDict comparableLeft comparableRight
removeRight right pairDict=
  leftOf right pairDict
  |>Maybe.map
      (\left->
        remove { left= left, right= right } pairDict
      )
  |>Maybe.withDefault
      pairDict

remove:
  Pair comparableLeft comparableRight
  ->PairDict comparableLeft comparableRight
  ->PairDict comparableLeft comparableRight
remove
  { left, right } (PairDict pairDict)
  =
  let
    { fromLeft, fromRight }= pairDict
  in
  PairDict
    {pairDict
    | fromLeft= Dict.remove left fromLeft
    , fromRight= Dict.remove right fromRight
    }

{-| map pairs

    digitNames=
      fromList
        [ ( 0, "zero" )
        , ( 1, "one" )
        ]
    mathSymbolNames=
      digitNames
      |>map
          (\{left,right}->
            { left= String.fromInt left
            , right= right
            }
          )
      |>insert { left= "+", right= "plus" }
-}
map:
  (Pair comparableLeft comparableRight
  ->Pair comparableResultLeft comparableResultRight
  )
  ->PairDict comparableLeft comparableRight
  ->PairDict comparableResultLeft comparableResultRight
map alter=
  foldl (alter >>insert) empty


{-| What has been left → right, right → left.

    upperToLower=
      fromList
        [ ( 'A', 'a' ), ( 'B', 'b' ) ]
    lowerToUpper=
      upperToLower |>swapLeftRight
-}
swapLeftRight:
  PairDict left right ->PairDict right left
swapLeftRight (PairDict pairDict)=
  PairDict
    { fromLeft= .fromRight pairDict
    , fromRight= .fromLeft pairDict
    }


{-| convert to a `Dict`, which you can access only from the left

    casedLetters=
      fromList
        [ ( 'A', 'a' ), ( 'B', 'b' ) ]
    lowerFromUpper=
      casedLetters |>toDict
-}
toDict: PairDict left right ->Dict left right
toDict (PairDict pairDict)=
  pairDict.fromLeft

