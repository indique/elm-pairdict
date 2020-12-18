module PairDict exposing
  ( PairDict
  , equal
  , empty, fromDict, fromList
  , access, emptyOrMore, size
  , values
  , putIn, map
  , remove
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
@docs equal, access, emptyOrMore, size

## in
@docs putIn, union

## out
@docs remove

## shape
@docs values, fold, map, toDict, encode
-}

import AssocList as AssocDict

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


{-| Want to look up value-pairs from the left or the right?

> You want the pair where `🗝️` is `1` and the pair where `🔑` is `0`?

      → < 🔑= 0, 🗝️= 2 >
        < 🔑= 2, 🗝️= 0 >
        < 🔑= 1, 🗝️= 1 > ←

> Going through while checking every pair, if `🗝️` is equal, then, if `🔑` is equal... Ah! Here they are:

        🔑 is 1 where 🗝️ is 1  and   🗝️ is 2 where 🔑 is 0

Like [assoc-list](https://github.com/pzp1997/assoc-list),
a `PairDict` allows for anything as values except for functions and things that contain functions.

## Example: cased letters
    type alias CasedLetter=
      { lowercase: Char
      , uppercase: Char
      }

    casedLetters: PairDict CasedLetter Char Char
    casedLetters=
      PairDict.empty .lowercase .uppercase
      |>PairDict.putIn { lowercase= 'a', uppercase= 'A' }
      |>PairDict.putIn { lowercase= 'b', uppercase= 'B' }
      |>PairDict.putIn { lowercase= 'c', uppercase= 'C' }

    uppercase char=
      PairDict.access .lowercase char lowerUppercaseLetters
      |>Maybe.map .uppercase
-}
type PairDict pair left right=
  Pairs
    { list: List pair
    , accessLeft: pair ->left
    , accessRight: pair ->right
    }


{-| using built-in (==) equality is often not useful in the context of association-dicts.

Do these 2 `PairDict`s have the same size and identical pairs (ignoring insertion order)?

    letterCodes=
      PairDict.fromList .letter .code 
        [ { letter= 'a', code= 97 }
        , { letter= 'b', code= 98 }
        ]
    fancyCompetingLetterCodes=
      PairDict.empty .code .letter
      |>PairDict.putIn { code= 98, letter= 'b' }
      |>PairDict.putIn { code= 97, letter= 'a' }
    
    PairDict.equal
      letterCodes
      fancyCompetingLetterCodes
> `True`
-}
equal:
  PairDict pair leftA rightA
  ->PairDict pair leftB rightB
  ->Bool
equal pairDictA=
  emptyOrMore
    { ifEmpty= isEmpty pairDictA
    , ifMore=
        \pair more->
          equal
            (pairDictA |>remove identity pair)
            more
    }

{-| A `PairDict` with no pairs inside
-}
empty:
  (pair ->left) ->(pair ->right)
  ->PairDict pair left right
empty accessLeft accessRight=
  Pairs
    { list= []
    , accessLeft= accessLeft
    , accessRight= accessRight
    }

{-| Create a `PairDict` from a association-`Dict`. `left` is the key, `right` is the value.
If multiple equal keys or values are present, the value **first** in the `Dict` is **prefered** (see `putIn`).
    
    lowerToUpperLetters=
      AssocList.empty
      |>AssocList.insert 'a' 'A'
      |>AssocList.insert 'b' 'B'

    lowerUpperLetters=
      PairDict.fromDict
        (\k v-> { lowercase= k, uppercase= v })
        .lowercase .uppercase
        lowerToUpperLetters
-}
fromDict:
  (left ->right ->pair)
  ->(pair ->left) ->(pair ->right)
  ->AssocDict.Dict left right
  ->PairDict pair left right
fromDict pairFromLeftRight accessLeft accessRight=
  AssocDict.foldl
    (\k v-> putIn (pairFromLeftRight k v))
    (empty accessLeft accessRight)

{-| Create a `PairDict` _conveniently_ from pairs.
If right or left values are given multiple times, the value **first** in the `List` is **prefered** (see `putIn`).
    
    PairDict.fromList
      [ { lowercase= 'b', uppercase= 'B' } --put in
      , { lowercase= 'a', uppercase= 'A' } --put in
      , { lowercase= 'b', uppercase= 'C' }
          --ignored, as the left value already exists
      , { lowercase= 'c', uppercase= 'A' }
          --ignored, as the right value already exists
      , { lowercase= 'c', uppercase= 'C' } --put in
      ]
-}
fromList:
  (pair ->left) ->(pair ->right)
  ->List pair ->PairDict pair left right
fromList accessLeft accessRight=
  empty accessLeft accessRight
  |>List.foldl putIn


{-| `Just` the pair in which `key` is present in the `PairDict`,
if no pair with the `key` is found `Nothing`.

    casedLetters=
      PairDict.empty .lowercase .uppercase
      |>PairDict.putIn { lowercase= 'a', uppercase= 'A' }
      |>PairDict.putIn { lowercase= 'b', uppercase= 'B' }

    lowercase char=
      PairDict.access .uppercase char
        casedLetters
      |>Maybe.map .lowercase
    uppercase char=
      PairDict.access .lowercase char
        casedLetters
      |>Maybe.map .uppercase

**Note**: If `accessKey` is neither `accessLeft` or `accessRight` (see `empty`, `fromList`, `fromDict`),
`access` will find the most recently inserted value where `key` is equal in the pair.

    PairDict.empty .lowercase .uppercase
    |>PairDict.putIn { inAlphabet= 0, lowercase= 'a', uppercase= 'A' }
    |>PairDict.putIn { inAlphabet= 1, lowercase= 'b', uppercase= 'B' }
    |>PairDict.access .inAlphabet 1
> `{ inAlphabet= 1, lowercase= 'b', uppercase= 'B' }`
-}
access:
  (pair ->key) ->key
  ->PairDict pair left right
  ->Maybe pair
access accessKey key ((Pairs pairs) as pairDict)=
  pairDict
  |>emptyOrMore
      { ifEmpty= Nothing
      , ifMore=
          \pair rest->
            case (==) (accessKey pair) key of
              True->
                Just pair
                
              False->
                access accessKey key rest
      }


{-| `ifEmpty` if the `PairDict` contains no pairs,
else `ifMore` with the most recently putIned pair followed by a `PairDict` with the other pairs.

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
    removeMostRecent pairDict=
      pairDict
      |>PairDict.emptyOrMore
          { ifMore= \_ rest-> rest
          , ifEmpty= pairDict
          }
-}
emptyOrMore:
  { ifEmpty: result
  , ifMore:
      pair ->PairDict pair left right
      ->result
  }
  ->PairDict pair left right ->result
emptyOrMore
  { ifEmpty, ifMore } (Pairs pairs)
  =
  case pairs.list of
    []->
      ifEmpty
    
    pair ::rest->
      ifMore pair
        (Pairs
          {pairs
          | list= rest
          }
        )

{-| **not exposed**, because the usage of `emptyOrMore` should be encouraged
-}
isEmpty: PairDict pair left right ->Bool
isEmpty=
  emptyOrMore
    { ifEmpty= True
    , ifMore= \_ _-> False
    }
      

{-| How many pairs there are in a `PairDict`.

    PairDict.fromList .number .following
      (List.map (\i-> { number= i, following= i+1 })
        (List.range 0 41)
      )
    |>PairDict.size
> `42`
-}
size: PairDict pair left right ->Int
size (Pairs pairs)=
  List.length pairs.list

{-| Values on the pairs.

    brackets=
      PairDict.fromList .open .closed
        [ { open= '(', closed= ')' }
        , { open= '{', closed= '}' }
        ]
    
    open= values .open brackets
    closed= values .closed brackets
-}
values:
  (pair ->value)
  ->PairDict pair left right ->List value
values accessValue (Pairs pairs)=
  List.map accessValue pairs.list

rightMember:
  right ->PairDict pair left right ->Bool
rightMember right ((Pairs pairs) as pairDict)=
  List.member right
  <|values pairs.accessRight pairDict

leftMember:
  left ->PairDict pair left right ->Bool
leftMember left ((Pairs pairs) as pairDict)=
  List.member left
  <|values pairs.accessLeft pairDict

{-| Put in a pair.

If either **value** is already **present**, the `PairDict` is **unchanged**.

    specialCasedA=
      { lowercase= 'a', uppercase= 'A', inAphabet= 0 }
    
    casedBadB=
      { lowercase= 'b', uppercase= 'B', inAlphabet= 0 }

    PairDict.empty .lowercase .uppercase --lowercase and uppercase are unique across each pair
    |>PairDict.putIn casedBadB --put in 
    |>PairDict.putIn specialCasedA --put in, because inAlphabet isn't checked
    |>PairDict.putIn { lowercase= 'b', uppercase= 'C' }
        --ignored, the left value already exists
    |>PairDict.putIn { lowercase= 'c', uppercase= 'A' }
        --ignored, the right value already exists
    |>PairDict.putIn { lowercase= 'c', uppercase= 'C' } --put in
-}
putIn:
  pair
  ->PairDict pair left right
  ->PairDict pair left right
putIn pair ((Pairs pairs) as pairDict)=
  case
    (||)
      (leftMember (pairs.accessLeft pair) pairDict)
      (rightMember (pairs.accessRight pair) pairDict)
    of
    True->
      Pairs pairs

    False->
      Pairs
        {pairs
        | list= pair ::pairs.list
        }


{-| Combine 2 `PairDict`s, so that the pairs in `toInsert` are put into `preferred`.
If a value on the left or right is present, prefer the last `PairDict` (see `putIn`).

    numberNamedOperators=
      PairDict.fromList
        [ { operator= '+', name= "plus" }
        , { operator= '-', name= "minus" }
        ]
    customNamedOperators=
      PairDict.fromList
        [ { operator= '∧', name= "and" }
        , { operator= '∨', name= "or" }
        , { operator= '-', name= "negate" }
        ]
    validNamedOperators=
      PairDict.union
        custumNamedOperators --has a '-' left
        numberOperatorNames --preferred → its '-'-pair is put in
-}
union:
  PairDict pair left right
  ->PairDict pair left right
  ->PairDict pair left right
union toInsert preferred=
  (fold putIn preferred) toInsert

{-| Reduce the left-right pairs
from most recently putIned to least recently putIned.

A fold in the other direction doesn't exist, as association-`Dict`s should rarely rely on order (see `equal`).

    brackets=
      PairDict.empty
      |>PairDict.putIn ( '(', ')' )
      |>PairDict.putIn ( '{', '}' )

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
  (pair ->acc ->acc)
  ->acc ->PairDict pair left right ->acc
fold reduce initial (Pairs pairs)=
  pairs.list
  |>List.foldl reduce
      initial


{-| Remove the left-right pair at `left`.
If **the value does not exist**, the `PairDict` is **unchanged**

    openClosedBrackets=
      PairDict.empty .open .closed
      |>PairDict.putIn { open= "(", closed= ")" }

    openClosedBrackets
    |>PairDict.remove .open ")" 
        --unchanged, ")" is not a open value
    |>PairDict.remove .open "("
> `PairDict.empty`

    openClosedBrackets
    |>PairDict.remove .closed "("
        --unchanged, "(" is not a closed value
    |>PairDict.remove .closed ")"
> `PairDict.empty`

**Notice:** If you don't specify `accessValue` as `left` or `right`, it acts as a normal filter

    PairDict.empty .open .closed
    |>PairDict.putIn { open= "(", closed= ")", meaning= Nothing }
    |>PairDict.putIn { open= "[", closed= "]", meaning= Just (List Element) }
    |>PairDict.putIn { open= "<, closed= ">", meaning= Nothing }
    |>PairDict.remove .meaning Nothing
-}
remove:
  (pair ->key) ->key
  ->PairDict pair left right
  ->PairDict pair left right
remove accessKey key (Pairs pairs)=
  Pairs
    {pairs
    | list=
        List.filter
          (\pair-> (/=) (accessKey pair) key)
          pairs.list
    }

{-| Map pairs. Take a look at pair's map operations.

    digitNames=
      PairDict.empty .number .name
      |>PairDict.putIn { number= 0, name= "zero" }
      |>PairDict.putIn { number= 1, name= "one" }

    mathSymbolNames=
      digitNames
      |>PairDict.map .symbol .name
          (\{ number, name }->
            { symbol= String.fromInt number, name= name }
          )
      |>PairDict.putIn { symbol= "+", name= "plus" }
-}
map:
  (pair ->resultPair)
  ->(resultPair ->resultLeft) ->(resultPair ->resultRight)
  ->PairDict pair left right
  ->PairDict resultPair resultLeft resultRight
map alter accessLeft accessRight=
  empty accessLeft accessRight
  |>fold (alter >>putIn)


{-| Convert a `PairDict` to an association-`Dict`, which you can **access** only **from the left**.

    casedLetters=
      PairDict.fromList .lowercase .uppercase
        [ { uppercase= 'A', lowercase= 'a' }
        , { uppercase= 'B', lowercase= 'b' }
        ]
    lowerFromUpper=
      PairDict.toDict casedLetters
-}
toDict:
  PairDict pair left right
  ->AssocDict.Dict left right
toDict (Pairs pairs)=
  (Pairs pairs)
  |>fold
      (\pair->
        AssocDict.insert
          (pairs.accessLeft pair)
          (pairs.accessRight pair)
      )
      AssocDict.empty


{-to contributers:
  for encoding / decoding: https://ellie-app.com/bPXzkZZHwyQa1
-}
{-| all left-right-pairs. **Not exposed**
-}
toPairs:
  PairDict pair left right ->List pair
toPairs (Pairs pairs)=
  pairs.list

{-| Convert a `PairDict` to a `Json.Encode.Value`.

    somePairDict=
      PairDict.empty
      |>PairDict.putIn ( 1, 11 )
      |>PairDict.putIn ( 2, 22 )
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
  (pair ->Encode.Value)
  ->PairDict pair left right ->Encode.Value
encode encodePair=
  toPairs
  >>Encode.list encodePair

{-| A `Json.Decode.Decoder` for `PairDict`s encoded by `encodePair`.

The order of insertion is not reconstructed (see `equal`)

    type alias NamedNumber=
      { number: Int
      , name: String
      }
    
    decodeNamedNumber=
      Decode.map NamedNumber
        (\{ number, name }->
          Decode.object
            [ ( "number", Decode.int number )
            , ( "name", Decode.string name )
            ]
        )

    """
    [
     {
      \"left\": 2,
      \"right\": "two"
     },
     {
      \"left\": 1,
      \"right\": "one"
     }
    ]
    """
    |>Decode.decodeString
        (PairDict.decode .number .name
          decodeNamedNumber
        )

> `Ok (Pairs [ { number= 1, name= "one" }, { number= 2, name= "two" } ])`
> = a `PairDict`
-}
decode:
  (pair ->left) ->(pair ->right)
  ->Decoder pair
  ->Decoder (PairDict pair left right)
decode accessLeft accessRight decodePair=
  Decode.map (fromList accessLeft accessRight)
    (Decode.list decodePair)

