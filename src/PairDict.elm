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

> you want the left value of `🗝️ 1` and the right value of `🔑 0`?

      → ( 🔑 0, 🗝️ 2 )
        ( 🔑 2, 🗝️ 0 )
        ( 🔑 1, 🗝️ 1 ) ←

> Going through while checking, if your key is equal... Ah! Here they are:

        🔑 1 and 🗝️ 2

Like [assoc-list](https://github.com/pzp1997/assoc-list),
a `PairDict` allows for anything as `left` or `right` values except for functions and things that contain functions.

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
  PairList (List (Pair left right))


{-| using (==) built-in equality is often not useful in the context of association-dicts.

Ignoring insertion order: do these 2 `PairDict`s have the same size and identical `Pair`s?

    letterCodes=
      PairDict.fromList
        [ ( 'a', 97 ), ( 'b', 98 ) ]
    fancyCompetingLetterCodes=
      empty
      |>insert ( 'b', 98 )
      |>insert ( 'a', 97 )
    
    equal
      letterCodes
      fancyCompetingLetterCodes

> `True`
-}
equal:
  PairDict left right ->PairDict left right ->Bool
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
  PairList []

{-| Create a `PairDict` from a association-`Dict`. `left` is the key, `right` is the value.
If multiple equal keys or values are present, the value **first** in the `Dict` is **prefered**.
    
    lowerToUpperLetters=
      AssocList.Dict.empty
      |>AssocList.Dict.insert 'a' 'A'
      |>AssocList.Dict.insert 'b' 'B'

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

{-| Create a `PairDict` _conveniently_ from `Tuple`s. `left` is the first, `right` is the second.
If right or left values are given multiple times, the value **first** in the `List` is **prefered**.
    
    fromList
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


{-| `Just` the right value if the left value is present in the `PairDict`, else `Nothing`

    casedLetters=
      empty
      |>insert ( 'a', 'A' )
      |>insert ( 'b', 'B' )

    lowerCase char=
      leftOf char casedLetters
-}
rightOf:
  left ->PairDict left right ->Maybe right
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

{-| `Just` the right value if the left value is present in the `PairDict`, else `Nothing`

    casedLetters=
      empty
      |>insert ( 'a', 'A' )
      |>insert ( 'b', 'B' )

    upperCase char=
      rightOf char casedLetters
-}
leftOf:
  right
  ->PairDict left right
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

    isEmpty=
      emptyOrMore
        { ifEmpty= True
        , ifMore= \_ _-> False
        }
    mostRecentlyInserted=
      emptyOrMore
        { ifMore= \pair _-> Just pair
        , ifEmpty= Nothing
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
  { ifEmpty, ifMore } (PairList pairList)
  =
  case pairList of
    []->
      ifEmpty
    
    pair ::rest->
      ifMore pair (PairList rest)
      

{-| How many pairs there are.

    size empty
> `0`
    
    size
      (fromList
        (List.range 0 41 |>List.map (\i-> ( i, i )))
      )
> `42`
-}
size: PairDict left right ->Int
size (PairList pairList)=
  List.length pairList

{-| values on the left

    brackets=
      fromList [ ( '(', ')' ), ( '{', '}' ) ]
    opening= lefts brackets
-}
lefts: PairDict left right ->List left
lefts (PairList pairList)=
  List.map leftIn pairList

{-| values on the right

    brackets=
      fromList [ ( '(', ')' ), ( '{', '}' ) ]
    closing= rights brackets
-}
rights: PairDict left right ->List right
rights (PairList pairList)=
  List.map rightIn pairList

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

    empty
    |>insert ( 'b', 'B' ) --puts it in 
    |>insert ( 'a', 'A' ) --puts it in
    |>insert ( 'b', 'C' ) --ignored, the left value already exists
    |>insert ( 'c', 'A' ) --ignored, the right value already exists
    |>insert ( 'c', 'C' ) --puts it in
-}
insert:
  Pair left right
  ->PairDict left right
  ->PairDict left right
insert pair ((PairList pairList) as pairDict)=
  case
    (||)
      (leftMember (leftIn pair) pairDict)
      (rightMember (rightIn pair) pairDict)
    of
    True->
      PairList pairList

    False->
      PairList (pair ::pairList)


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
  fold insert preferred added

{-| Reduce the left-right `Pair`s from most recently inserted
to least recently inserted.
A fold in the other direction doesn't exist, as association-`Dict`s should rarely rely on order (see `equal`)

    brackets=
      empty
      |>insert ( '(', ')' )
      |>insert ( '{', '}' )

    openingAndClosing=
      brackets
      |>fold
          (\( left, right ) acc->
            acc ++[ String.fromList [ left, right ] ]
          )
          []
> `[ "{}", "()" ]`
-}
fold:
  (Pair left right ->acc ->acc)
  ->acc ->PairDict left right ->acc
fold reduce initial (PairList pairList)=
  pairList
  |>List.foldl
      (\pair-> reduce pair)
      initial


{-| remove left-right pair.
If **`left` does not exist**, the `PairDict` is **unchanged**

    empty
    |>insert "(" ")"
    |>removeLeft ")" --unchanged, ")" is not a left value
    |>removeLeft "(" --removes ( "(", ")" )
    --empty
-}
removeLeft:
  left
  ->PairDict left right
  ->PairDict left right
removeLeft left (PairList pairList)=
  List.filter (leftIn >>(/=) left) pairList
  |>PairList

{-| remove left-right pair.
If `right` does not exist, the `PairDict` is unchanged

    empty
    |>insert ( "(", ")" )
    |>removeRight "(" --unchanged, "(" is not a right value
    |>removeRight ")" --removes ( "(", ")" )
    --empty
-}
removeRight:
  right
  ->PairDict left right
  ->PairDict left right
removeRight right (PairList pairList)=
  List.filter (rightIn >>(/=) right) pairList
  |>PairList
  

{-| map pairs

    digitNames=
      empty
      |>insert ( 0, "zero" )
      |>insert ( 1, "one" )

    mathSymbolNames=
      digitNames
      |>map (Pair.mapLeft String.fromInt)
      |>insert ( "+", "plus" )
-}
map:
  (Pair left right
  ->Pair resultLeft resultRight
  )
  ->PairDict left right
  ->PairDict resultLeft resultRight
map alter=
  fold (alter >>insert) empty


{-| Convert to an association-`Dict`, which you can access only from the left

    casedLetters=
      fromList
        [ ( 'A', 'a' ), ( 'B', 'b' ) ]
    lowerFromUpper=
      casedLetters |>dictFromLeft
-}
toDict:
  PairDict left right ->AssocDict.Dict left right
toDict=
  fold
    (\( left, right )-> AssocDict.insert left right)
    AssocDict.empty



--for encoding / decoding: https://ellie-app.com/bNL2HsCtswQa1
{-| all left-right-`Pair`s. **Not exposed**
-}
pairs: PairDict left right ->List (Pair left right)
pairs (PairList pairList)=
  pairList

{-| Convert a `PairDict` to a `Json.Encode.Value`.

    somePairDict=
      empty
      |>insert ( 1, 11 )
      |>insert ( 2, 22 )
    Encode.encode 1
      (encode
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

> `Ok (fromList [ ( 1, 11 ), ( 2, 22 ) ])`
-}
decode:
  Decoder left ->Decoder right
  ->Decoder (PairDict left right)
decode decodeLeft decodeRight=
  Decode.map fromList
    (Decode.list
      (Pair.decode decodeLeft decodeRight)
    )

