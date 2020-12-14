module PairDict exposing
  ( PairDict
  , equal
  , empty, fromDict, fromList
  , rightOf, leftOf, size
  , lefts, rights
  , insert, map
  , removeLeft, removeRight
  , fold
  , union
  , swapLeftRight
  , dictFromLeft, dictFromRight
  , encode, decode
  )
{-| 
@docs PairDict

@docs equal

## create
@docs empty, fromDict, fromList, decode

## access
@docs rightOf, leftOf

## properties
@docs size

## in
@docs insert, union

## out
@docs removeLeft, removeRight

## shape
@docs lefts, rights, swapLeftRight, fold, map, dictFromLeft, dictFromRight, encode
-}

import AssocList as AssocDict
import Pair exposing (Pair)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


{-| Want to look up value-pairs from the left or the right?

Like [assoc-list](https://github.com/pzp1997/assoc-list),
a `PairDict` allows for anything as `left` or `right` values except for functions and things that contain functions.

## Example: cased letters
    lowerUppercaseLetters=
      empty
      |>insert { left= 'a', right= 'A' }
      |>insert { left= 'b', right= 'B' }
      |>insert { left= 'c', right= 'C' }

    upperCase char=
      rightOf char lowerUppercaseLetters
-}
type PairDict left right=
  PairDict
    { fromLeft: AssocDict.Dict left right
    , fromRight: AssocDict.Dict right left
    }

{-| using (==) built-in equality is often not useful in the context of association-dicts.

Ignoring insertion order: do these 2 `PairDict`s have the same size and identical `Pair`s?

    letterCodes=
      fromList [ ( 'a', 0 ), ( 'b', 1 ) ]
    fancyCompetingLetterCodes=
      fromList [ ( 'b', 1 ), ( 'a', 0 ) ]
    
    equal
      letterCodes
      fancyCompetingLetterCodes
    --> True
-}
equal:
  PairDict left right ->PairDict left right ->Bool
equal pairDictA pairDictB=
  AssocDict.eq
    (dictFromLeft pairDictA)
    (dictFromLeft pairDictB)

{-| A `PairDict` with no elements
-}
empty: PairDict left right
empty=
  PairDict
    { fromLeft= AssocDict.empty
    , fromRight= AssocDict.empty
    }

{-| Create a `PairDict` conveniently from `Pair`-`Tuple`s. `left` is the first, `right` is the second.
If right or left values are given multiple times, the value **first** in the `List` is **prefered**.
    
    lowerToUpperLetters=
      AssocList.Dict.empty
      |>AssocList.Dict.insert 'a' 'A'
      |>AssocList.Dict.insert 'b' 'B'

    lowerUpperLetters= fromDict lowerToUpperLetters
-}
fromDict:
  AssocDict.Dict left right ->PairDict left right
fromDict=
  AssocDict.foldl
    (\k v-> insert ({ left= k, right= v }))
    empty

{-| Create a `PairDict` _conveniently_ from `Tuple`s. `left` is the first, `right` is the second.
If right or left values are given multiple times, the value **first** in the `List` is **prefered**.
    
    fromList
      [ ( 'b', 'B' ) --insert { left= 'b', right= 'B' }
      , ( 'a', 'A' ) --insert { left= 'a', right= 'A' }
      , ( 'b', 'C' ) --ignored, as the left value already exists
      , ( 'c', 'A' ) --ignored, as the right value already exists
      , ( 'c', 'C' ) --insert { left= 'c', right= 'C' }
      ]
-}
fromList:
  List ( left, right ) ->PairDict left right
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
      leftOf char casedLetters
-}
rightOf:
  left
  ->PairDict left right
  ->Maybe right
rightOf left (PairDict pairDict)=
  AssocDict.get left pairDict.fromLeft

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
  right
  ->PairDict left right
  ->Maybe left
leftOf right (PairDict pairDict)=
  AssocDict.get right pairDict.fromRight

{-| How many pairs there are.

    size empty --0
    
    size
      (fromList
        (List.range 0 41 |>List.map (\i-> ( i, i )))
      )
    --42
-}
size: PairDict left right ->Int
size (PairDict pairDict)=
  AssocDict.size pairDict.fromLeft

{-| values on the left

    brackets=
      fromList [ ( '(', ')' ), ( '{', '}' ) ]
    opening= lefts brackets
-}
lefts: PairDict left right ->List left
lefts=
  AssocDict.keys <<dictFromLeft

{-| values on the right

    brackets=
      fromList [ ( '(', ')' ), ( '{', '}' ) ]
    closing= rights brackets
-}
rights: PairDict left right ->List right
rights=
  AssocDict.keys <<dictFromRight

{-| Put in a left-right-`Pair`.

If either **value** is already **present**, the `PairDict` is **unchanged**.

    empty
    |>insert { left= "(", right= ")" }
    |>insert { right= "fire", left= "water" }
    --fromList
    --  [ ( "(", ")" ) ( "water", "fire" ) ]
-}
insert:
  Pair left right
  ->PairDict left right
  ->PairDict left right
insert { left, right } pairDict=
  let
    fromLeft= dictFromLeft pairDict
    fromRight= dictFromRight pairDict
  in
  case
    (||)
      (AssocDict.member left fromLeft)
      (AssocDict.member right fromRight)
    of
    True->
      pairDict

    False->
      PairDict
        { fromLeft=
            AssocDict.insert left right
              fromLeft
        , fromRight=
            AssocDict.insert right left
              fromRight
        }


{-| Merge 2 `PairDict`s.
If a value on the left or right is present, prefer the second dict.

    numberNamedOperators=
      fromList
        [ ( '+', "plus" )
        , ( '-', "minus" )
        ]
    customNamedOperators=
      fromList
        [ ( '∧', "and" )
        , ( '∨', "or" )
        , ( '-', "negate" )
        ]
    validNamedOperators=
      union
        custumNamedOperators --has a '-' left
        numberOperatorNames --preferred → its '-'-Pair is inserted
-}
union:
  PairDict left right
  ->PairDict left right
  ->PairDict left right
union added preferred=
  preferred
  |>fold insert added

{-| Reduce the left-right `Pair`s from most recently inserted
to least recently inserted.
A fold in the other direction doesn't exist, as association-`Dict`s should rarely rely on order (see `equal`)

    brackets=
      fromList
        [ ( '(', ')' )
        , ( '{', '}' )
        ]
    openingAndClosing=
      brackets
      |>fold
          (\{ left, right } acc->
            acc ++[ String.fromList [ left, right ] ]
          )
          []
      --[ "{}", "()" ]
-}
fold:
  (Pair left right ->acc ->acc)
  ->acc ->PairDict left right ->acc
fold reduce initial=
  dictFromLeft
  >>AssocDict.foldl
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
  left
  ->PairDict left right
  ->PairDict left right
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
  right
  ->PairDict left right
  ->PairDict left right
removeRight right pairDict=
  leftOf right pairDict
  |>Maybe.map
      (\left->
        remove { left= left, right= right } pairDict
      )
  |>Maybe.withDefault
      pairDict

remove:
  Pair left right
  ->PairDict left right
  ->PairDict left right
remove
  { left, right } (PairDict pairDict)
  =
  let
    { fromLeft, fromRight }= pairDict
  in
  PairDict
    {pairDict
    | fromLeft= AssocDict.remove left fromLeft
    , fromRight= AssocDict.remove right fromRight
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
  (Pair left right
  ->Pair resultLeft resultRight
  )
  ->PairDict left right
  ->PairDict resultLeft resultRight
map alter=
  fold (alter >>insert) empty


{-| What has been left → right, right → left.

    upperToLower=
      fromList
        [ ( 'A', 'a' ), ( 'B', 'b' ) ]
    lowerToUpper=
      upperToLower |>swapLeftRight
-}
swapLeftRight:
  PairDict left right ->PairDict right left
swapLeftRight pairDict=
  PairDict
    { fromLeft= dictFromRight pairDict
    , fromRight= dictFromLeft pairDict
    }


{-| No cost: An association-`Dict`, which you can access only from the left

    casedLetters=
      fromList
        [ ( 'A', 'a' ), ( 'B', 'b' ) ]
    lowerFromUpper=
      casedLetters |>dictFromLeft
-}
dictFromLeft:
  PairDict left right ->AssocDict.Dict left right
dictFromLeft (PairDict pairDict)=
  pairDict.fromLeft

{-| No cost: An association-`Dict`, which you can access only from the left

    casedLetters=
      fromList
        [ ( 'A', 'a' ), ( 'B', 'b' ) ]
    upperFromLower=
      casedLetters |>dictFromRight
-}
dictFromRight:
  PairDict left right ->AssocDict.Dict right left
dictFromRight (PairDict pairDict)=
  pairDict.fromRight



--for encoding / decoding: https://ellie-app.com/bNKgXj4Tw5Ca1
{-| all left-right-`Pair`s. As its only practical use is in `decode`, this is not exposed
-}
pairs: PairDict left right ->List (Pair left right)
pairs=
  dictFromLeft >>AssocDict.toList
  >>List.map
      (\( first, second )->
        { left= first, right= second }
      )

{-| Convert a `PairDict` to a `Json.Encode.Value`.

    somePairDict=
      empty
      |>insert { left= 1, right= 11 }
      |>insert { left= 2, right= 22 }
    Encode.encode 1
      (encode
        Encode.int Encode.int
        somePairDict
      )
    {->
    """
    [
     {
      \"left\": 2,
      \"right\": 22
     },
     {
      \"left\": 1,
      \"right\": 11
     }
    ]
    """
    -}
-}
encode:
  (left ->Encode.Value) ->(right ->Encode.Value)
  ->PairDict left right ->Encode.Value
encode encodeLeft encodeRight=
  pairs
  >>Encode.list
      (Pair.encode encodeLeft encodeRight)

{-| A `Json.Decode.Decoder` for `PairDict`s, encoded by `encodePair`.

The order of insertion is not reconstructed (see `equal`)

    """
    [
     {
      \"left\": 2,
      \"right\": 22
     },
     {
      \"left\": 1,
      \"right\": 11
     }
    ]
    """
    |>Decode.decodeString
        (decode Decode.int Decode.int)
    {->
    Ok (fromList [ ( 1, 11 ), ( 2, 22 ) ])
    -}
-}
decode:
  Decoder left ->Decoder right
  ->Decoder (PairDict left right)
decode decodeLeft decodeRight=
  Decode.map fromList
    (Decode.list
      (Decode.map (\{ left, right }-> ( left, right ))
        (Pair.decode decodeLeft decodeRight)
      )
    )