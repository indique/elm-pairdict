module Pair exposing
  ( Pair
  , leftIn, rightIn
  , mapLeft, mapRight
  , encode, decode
  )
{-|
@docs Pair

### create
@docs decode

or through the constructor

### access
@docs leftIn, rightIn

### shape
@docs mapLeft, mapRight, encode
-}

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


{-| `( left, right )` as a value-pair.

    openClosedCurlyBracePair=
      ( '{', '}' )
-}
type alias Pair left right=
  ( left, right )

{-| access the left value in a `Pair`.

    openClosedCurlyBracePair=
      ( '{', '}' )
    openCurly= leftIn openClosedCurlyBracePair
-}
leftIn: Pair left right ->left
leftIn ( left, right )=
  left

{-| access the right value in a `Pair`.

    openClosedCurlyBracePair=
      ( '{', '}' )
    closedCurly= rightIn openClosedCurlyBracePair
-}
rightIn: Pair left right ->right
rightIn ( left, right )=
  right


{-| map the `left` value in a `Pair`

    ( 128522, "smile" )
    |>mapLeft Char.fromCode

> `( '😊', "smile" )`

This is handy when using maps or folds etc.

    namedEmojiCodes=
      PairDict.empty
      |>PairDict.insert ( 127824, "pear" )
      |>PairDict.insert ( 127795, "tree" )
    namedEmojis=
      namedEmojiCodes
      |>PairDict.map (Pair.mapLeft String.fromInt)

> `PairDict.fromList [ ( '🍐', "pear" ), ( '🌳', "tree" ) ]`
-}
mapLeft:
  (left ->resultLeft)
  ->Pair left right
  ->Pair resultLeft right
mapLeft alter ( left, right )=
  ( alter left, right )

{-| map the `right` value in a `Pair`

    ( "smile", 128522 )
    |>mapRight Char.fromCode

> `( "smile", '😊' )`

This is handy when using maps or folds etc.

    namedEmojiCodes=
      PairDict.empty
      |>PairDict.insert ( "pear", 127824 )
      |>PairDict.insert ( "tree", 127795 )
    namedEmojis=
      namedEmojiCodes
      |>PairDict.map (Pair.mapLeft String.fromInt)

> `PairDict.fromList [ ( "pear", '🍐' ), ( "tree", '🌳' ) ]`
-}
mapRight:
  (right ->resultRight)
  ->Pair left right
  ->Pair left resultRight
mapRight alter ( left, right )=
  ( left, alter right )


{-| Convert a `Pair` to a `Json.Encode.Value`.

    Encode.encode 1
      (Pair.encode
        Encode.int Encode.int
        ( 1, 11 )
      )

    """
    {
     \"left\": 1,
     \"right\": 11
    }
    """
-}
encode:
  (left ->Encode.Value)
  ->(right ->Encode.Value)
  ->Pair left right ->Encode.Value
encode encodeLeft encodeRight ( left, right )=
  Encode.object
    [ ( "left", encodeLeft left )
    , ( "right", encodeRight right )
    ]

{-| A `Json.Decode.Decoder` for `Pair`s, encoded by `encodePair`
    
    """
    {
     \"left\": 1,
     \"right\": 11
    }
    """
    |>Decode.decodeString
        (Pair.decode Decode.int Decode.int)

> `Ok ( 1, 11 )`
-}
decode:
  Decoder left ->Decoder right
  ->Decoder (Pair left right)
decode decodeLeft decodeRight=
  Decode.map2 Tuple.pair
    (Decode.field "left" decodeLeft)
    (Decode.field "right" decodeRight)

