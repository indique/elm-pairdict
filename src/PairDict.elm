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
  , toDict
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
@docs lefts, rights, fold, map, toDict, encode
-}

import Dict
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
  PairList (List (Pair left right))


isEmpty: PairDict left right ->Bool
isEmpty=
  List.isEmpty <<pairs

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
equal pairDictA (PairList pairListB)=
  case pairListB of
    []->
      isEmpty pairDictA
    
    pair ::rest->
      case rightOf pair.left pairDictA of
        Nothing->
          False
        
        Just _->
          equal
            (pairDictA |>removeLeft pair.left)
            (PairList rest)


{-| A `PairDict` with no elements
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

    lowerUpperLetters= fromDict lowerToUpperLetters
-}
fromDict:
  AssocDict.Dict left right
  ->PairDict left right
fromDict=
  AssocDict.foldl
    (\k v-> insert { left= k, right= v })
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
    (\( first, second )->
      insert { left= first, right= second }
    )
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
rightOf left (PairList pairList)=
  case pairList of
    pair ::rest->
      case (==) pair.left left of
        True->
          Just pair.right

        False->
          rightOf left (PairList rest)
    
    []->
      Nothing

{-| `Just` the right value if the left value is present in the `PairDict`, else `Nothing`

    casedLetters=
      empty
      |>insert { left= 'a', right= 'A' }
      |>insert { left= 'b', right= 'B' }

    upperCase char=
      rightOf char casedLetters
-}
leftOf:
  right
  ->PairDict left right
  ->Maybe left
leftOf right (PairList pairList)=
  case pairList of
    pair ::rest->
      case (==) pair.right right of
        True->
          Just pair.left

        False->
          leftOf right (PairList rest)
    
    []->
      Nothing


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
  List.map .left pairList

{-| values on the right

    brackets=
      fromList [ ( '(', ')' ), ( '{', '}' ) ]
    closing= rights brackets
-}
rights: PairDict left right ->List right
rights (PairList pairList)=
  List.map .right pairList

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
    |>insert { left= 'b', right= 'B' } --puts it in 
    |>insret { right= 'A', left= 'a' } --puts it in
    |>insert { left= 'b', right= 'C' } --ignored, the left value already exists
    |>insert { left= 'c', right= 'A' } --ignored, the right value already exists
    |>insert { left= 'c', right= 'C' } --puts it in
-}
insert:
  Pair left right
  ->PairDict left right
  ->PairDict left right
insert pair ((PairList pairList) as pairDict)=
  PairList
    (case
      (||)
        (leftMember pair.left pairDict)
        (rightMember pair.right pairDict)
      of
      True->
        pairList

      False->
        pair ::pairList
    )


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
    |>removeLeft "(" --removes { left= "(", right= ")" }
    --empty
-}
removeLeft:
  left
  ->PairDict left right
  ->PairDict left right
removeLeft left (PairList pairList)=
  List.filter (.left >>(/=) left) pairList
  |>PairList

{-| remove left-right pair.
If `right` does not exist, the `PairDict` is unchanged

    empty
    |>insert { left= "(", right= ")" }
    |>removeRight "(" --unchanged, "(" is not a right value
    |>removeRight ")" --removes { left= "(", right= ")" }
    --empty
-}
removeRight:
  right
  ->PairDict left right
  ->PairDict left right
removeRight right (PairList pairList)=
  List.filter (.right >>(/=) right) pairList
  |>PairList
  

{-| map pairs

    digitNames=
      fromList
        [ ( 0, "zero" )
        , ( 1, "one" )
        ]
    mathSymbolNames=
      digitNames
      |>map (Pair.mapLeft String.fromInt)
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
    (\pair-> AssocDict.insert pair.left pair.right)
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
      |>insert { left= 1, right= 11 }
      |>insert { left= 2, right= 22 }
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
      (Decode.map (\{ left, right }-> ( left, right ))
        (Pair.decode decodeLeft decodeRight)
      )
    )

