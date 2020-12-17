module PairDict exposing
  ( PairDict
  , equal
  , empty, fromDict, fromList
  , rightOf, leftOf, emptyOrMore, size
  , lefts, rights
  , insert, map
  , removeLeft, removeRight
  , fold
  , union
  , toDict
  , encode, decode
  )
{-| 
@docs PairDict

## create
@docs empty, fromDict, fromList, decode

## scan
@docs rightOf, leftOf, equal, emptyOrMore, size

## in
@docs insert, union

## out
@docs removeLeft, removeRight

## shape
@docs lefts, rights, fold, map, toDict, encode
-}

import Dict
import AssocList as AssocDict
import Pair exposing (Pair, leftIn, rightIn)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


{-| Want to look up value-pairs from the left or the right?

> You want the left value of `🗝️ 1` and the right value of `🔑 0`?

      → ( 🔑 0, 🗝️ 2 )
        ( 🔑 2, 🗝️ 0 )
        ( 🔑 1, 🗝️ 1 ) ←

> Going through while checking, if your key is equal... Ah! Here they are:

        🔑 1 and 🗝️ 2

Like [assoc-list](https://github.com/pzp1997/assoc-list),
a `PairDict` allows for anything as left or right values except for functions and things that contain functions.

## Example: cased letters
    lowerUppercaseLetters=
      PairDict.empty
      |>PairDict.insert ( 'a', 'A' )
      |>PairDict.insert ( 'b', 'B' )
      |>PairDict.insert ( 'c', 'C' )

    upperCase char=
      rightOf char lowerUppercaseLetters
-}
type PairDict left right=
  Pairs (List (Pair left right))


{-| using (==) built-in equality is often not useful in the context of association-dicts.

Ignoring insertion order: do these 2 `PairDict`s have the same size and identical `Pair`s?

    letterCodes=
      PairDict.fromList
        [ ( 'a', 97 ), ( 'b', 98 ) ]
    fancyCompetingLetterCodes=
      PairDict.empty
      |>PairDict.insert ( 'b', 98 )
      |>PairDict.insert ( 'a', 97 )
    
    PairDict.equal
      letterCodes
      fancyCompetingLetterCodes
> `True`
-}
equal:
  PairDict left right ->PairDict left right
  ->Bool
equal pairDictA=
  emptyOrMore
    { ifEmpty=
        pairDictA
        |>emptyOrMore
            { ifEmpty= True
            , ifMore= \_ _-> False
            }
    , ifMore=
        \( left, _ ) more->
          case rightOf left pairDictA of
            Just _->
              equal
                (pairDictA |>removeLeft left)
                more
            
            Nothing->
              False
    }


{-| A `PairDict` with no `Pair`s inside
-}
empty: PairDict left right
empty=
  Pairs []

{-| Create a `PairDict` from a association-`Dict`. `left` is the key, `right` is the value.
If multiple equal keys or values are present, the value **first** in the `Dict` is **prefered** (see `insert`).
    
    lowerToUpperLetters=
      AssocList.empty
      |>AssocList.insert 'a' 'A'
      |>AssocList.insert 'b' 'B'

    lowerUpperLetters=
      PairDict.fromDict lowerToUpperLetters
-}
fromDict:
  AssocDict.Dict left right
  ->PairDict left right
fromDict=
  AssocDict.foldl
    (\k v-> insert ( k, v ))
    empty

{-| Create a `PairDict` _conveniently_ from `Pair`s.
If right or left values are given multiple times, the value **first** in the `List` is **prefered** (see `insert`).
    
    PairDict.fromList
      [ ( 'b', 'B' ) --insert ( 'b', 'B' )
      , ( 'a', 'A' ) --insert ( 'a', 'A' )
      , ( 'b', 'C' ) --ignored, as the left value already exists
      , ( 'c', 'A' ) --ignored, as the right value already exists
      , ( 'c', 'C' ) --insert ( 'c', 'C' )
      ]
-}
fromList:
  List (Pair left right) ->PairDict left right
fromList=
  List.foldl insert empty


{-| `Just` the right value if `left` is present in the `PairDict`, else `Nothing`

    casedLetters=
      PairDict.empty
      |>PairDict.insert ( 'a', 'A' )
      |>PairDict.insert ( 'b', 'B' )

    lowerCase char=
      leftOf char casedLetters
-}
rightOf:
  left ->PairDict left right
  ->Maybe right
rightOf left=
  emptyOrMore
    { ifEmpty= Nothing
    , ifMore=
        \( pairLeft, pairRight ) rest->
          case (==) pairLeft left of
            True->
              Just pairRight
              
            False->
              rightOf left rest
    }

{-| `Just` the left value if `right` is present in the `PairDict`, else `Nothing`

    casedLetters=
      PairDict.empty
      |>PairDict.insert ( 'a', 'A' )
      |>PairDict.insert ( 'b', 'B' )

    upperCase char=
      rightOf char casedLetters
-}
leftOf:
  right ->PairDict left right
  ->Maybe left
leftOf right=
  emptyOrMore
    { ifEmpty= Nothing
    , ifMore=
        \( pairLeft, pairRight ) rest->
          case (==) pairRight right of
            True->
              Just pairLeft

            False->
              leftOf right rest
    }


{-| `ifEmpty` if the `PairDict` contains no `Pair`s,
else `ifMore` with the most recently inserted `Pair` followed by a `PairDict` with the other `Pair`s.

It has a very similar use case to a `case` .. `of` on a `List`.

    isEmpty=
      PairDict.emptyOrMore
        { ifEmpty= True
        , ifMore= \_ _-> False
        }
    mostRecent=
      PairDict.emptyOrMore
        { ifMore= \pair _-> Just pair
        , ifEmpty= Nothing
        }
    removeMostRecent=
      PairDict.emptyOrMore
        { ifMore= \_ rest-> rest
        , ifEmpty= PairDict.empty
        }
-}
emptyOrMore:
  { ifEmpty: result
  , ifMore:
      Pair left right ->PairDict left right
      ->result
  }
  ->PairDict left right ->result
emptyOrMore
  { ifEmpty, ifMore } (Pairs pairs)
  =
  case pairs of
    []->
      ifEmpty
    
    pair ::rest->
      ifMore pair (Pairs rest)
      

{-| How many `Pair`s there are in a `PairDict`.

    PairDict.size empty
> `0`
    
    meaninglessNumbers=
      PairDict.fromList
        (List.map (\i-> ( i, i ))
          (List.range 0 41)
        )
    PairDict.size meaninglessNumbers
> `42`
-}
size: PairDict left right ->Int
size (Pairs pairs)=
  List.length pairs

{-| Values on the left of all `Pair`s.

    brackets=
      PairDict.fromList
        [ ( '(', ')' ), ( '{', '}' ) ]
    opening= lefts brackets
-}
lefts: PairDict left right ->List left
lefts (Pairs pairs)=
  List.map leftIn pairs

{-| Values on the right of all `Pair`s.

    brackets=
      PairDict.fromList
        [ ( '(', ')' ), ( '{', '}' ) ]
    closing= rights brackets
-}
rights: PairDict left right ->List right
rights (Pairs pairs)=
  List.map rightIn pairs

rightMember:
  right ->PairDict left right ->Bool
rightMember right=
  List.member right <<rights

leftMember:
  left ->PairDict left right ->Bool
leftMember left=
  List.member left <<lefts

{-| Put in a left-right-`Pair`.

If either **value** is already **present**, the `PairDict` is **unchanged**.

    PairDict.empty
    |>PairDict.insert ( 'b', 'B' )
        --puts it in 
    |>PairDict.insert ( 'a', 'A' )
        --puts it in
    |>PairDict.insert ( 'b', 'C' )
        --ignored, the left value already exists
    |>PairDict.insert ( 'c', 'A' )
        --ignored, the right value already exists
    |>PairDict.insert ( 'c', 'C' )
        --puts it in
-}
insert:
  Pair left right
  ->PairDict left right
  ->PairDict left right
insert pair ((Pairs pairs) as pairDict)=
  case
    (||)
      (leftMember (leftIn pair) pairDict)
      (rightMember (rightIn pair) pairDict)
    of
    True->
      Pairs pairs

    False->
      Pairs (pair ::pairs)


{-| Combine 2 `PairDict`s, so that the `Pair`s in `toInsert` get inserted into `preferred`.
If a value on the left or right is present, prefer the last `PairDict` (see `insert`).

    numberNamedOperators=
      PairDict.fromList
        [ ( '+', "plus" )
        , ( '-', "minus" )
        ]
    customNamedOperators=
      PairDict.fromList
        [ ( '∧', "and" )
        , ( '∨', "or" )
        , ( '-', "negate" )
        ]
    validNamedOperators=
      PairDict.union
        custumNamedOperators --has a '-' left
        numberOperatorNames --preferred → its '-'-Pair is inserted
-}
union:
  PairDict left right
  ->PairDict left right
  ->PairDict left right
union toInsert preferred=
  (fold insert preferred) toInsert

{-| Reduce the left-right `Pair`s
from most recently inserted to least recently inserted.

A fold in the other direction doesn't exist, as association-`Dict`s should rarely rely on order (see `equal`).

    brackets=
      PairDict.empty
      |>PairDict.insert ( '(', ')' )
      |>PairDict.insert ( '{', '}' )

    openingAndClosing=
      brackets
      |>PairDict.fold
          (\( left, right ) acc->
            acc++[ String.fromList [ left, right ] ]
          )
          []
> `[ "{}", "()" ]`
-}
fold:
  (Pair left right ->acc ->acc)
  ->acc ->PairDict left right ->acc
fold reduce initial (Pairs pairs)=
  pairs
  |>List.foldl
      (\pair-> reduce pair)
      initial


{-| Remove the left-right `Pair` at `left`.
If **`left` does not exist**, the `PairDict` is **unchanged**

    PairDict.empty
    |>PairDict.insert "(" ")"
    |>PairDict.removeLeft ")" 
        --unchanged, ")" is not a left value
    |>PairDict.removeLeft "("
        --removes ( "(", ")" )
> `PairDict.empty`
-}
removeLeft:
  left
  ->PairDict left right
  ->PairDict left right
removeLeft left (Pairs pairs)=
  List.filter (leftIn >>(/=) left) pairs
  |>Pairs

{-| Remove the left-right `Pair` at `right`.
If `right` does not exist, the `PairDict` is unchanged

    PairDict.empty
    |>PairDict.insert ( "(", ")" )
    |>PairDict.removeRight "("
        --unchanged, "(" is not a right value
    |>PairDict.removeRight ")"
        --removes ( "(", ")" )
> `PairDict.empty`
-}
removeRight:
  right
  ->PairDict left right
  ->PairDict left right
removeRight right (Pairs pairs)=
  List.filter (rightIn >>(/=) right) pairs
  |>Pairs
  

{-| Map `Pair`s. Take a look at `Pair`'s map operations.

    digitNames=
      PairDict.empty
      |>PairDict.insert ( 0, "zero" )
      |>PairDict.insert ( 1, "one" )

    mathSymbolNames=
      digitNames
      |>PairDict.map (Pair.mapLeft String.fromInt)
      |>PairDict.insert ( "+", "plus" )
-}
map:
  (Pair left right
  ->Pair resultLeft resultRight
  )
  ->PairDict left right
  ->PairDict resultLeft resultRight
map alter=
  fold (alter >>insert) empty


{-| Convert a `PairDict` to an association-`Dict`, which you can **access** only **from the left**.

    casedLetters=
      PairDict.fromList
        [ ( 'A', 'a' ), ( 'B', 'b' ) ]
    lowerFromUpper=
      PairDict.toDict casedLetters
-}
toDict:
  PairDict left right ->AssocDict.Dict left right
toDict=
  fold
    (\( left, right )-> AssocDict.insert left right)
    AssocDict.empty



{-to contributers:
  for encoding / decoding: https://ellie-app.com/bPXzkZZHwyQa1
-}
{-| all left-right-`Pair`s. **Not exposed**
-}
toPairs:
  PairDict left right
  ->List (Pair left right)
toPairs (Pairs pairs)=
  pairs

{-| Convert a `PairDict` to a `Json.Encode.Value`.

    somePairDict=
      PairDict.empty
      |>PairDict.insert ( 1, 11 )
      |>PairDict.insert ( 2, 22 )
    Encode.encode 1
      (PairDict.encode
        Encode.int Encode.int
        somePairDict
      )


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
encode:
  (left ->Encode.Value) ->(right ->Encode.Value)
  ->PairDict left right ->Encode.Value
encode encodeLeft encodeRight=
  toPairs
  >>Encode.list
      (Pair.encode encodeLeft encodeRight)

{-| A `Json.Decode.Decoder` for `PairDict`s encoded by `encodePair`.

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
        (PairDict.decode
          Decode.int Decode.int
        )

> `Ok (Pairs [ ( 1, 11 ), ( 2, 22 ) ])` = a `PairDict`
-}
decode:
  Decoder left ->Decoder right
  ->Decoder (PairDict left right)
decode decodeLeft decodeRight=
  Decode.map fromList
    (Decode.list
      (Pair.decode decodeLeft decodeRight)
    )

