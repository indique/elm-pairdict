module Pair exposing
  ( Pair
  , mapLeft, mapRight
  , encode, decode
  )
{-|
@docs Pair

### create
@docs decode

or through the constructor

### shape
@docs mapLeft, mapRight, encode
-}

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


{-| `left` & `right` as a value-pair

    openClosedCurlyBracePair=
      { left= '{', right= '}' }

    openCurly= .left openClosedCurlyBracePair
    closedCurly= .right openClosedCurlyBracePair
-}
type alias Pair left right=
  { left: left
  , right: right
  }

{-| map the `left` value in a `Pair`

    { left= 128522, right= "smile" }
    |>mapLeft Char.fromCode

> `{ left= '😊', right= "smile" }`

This is handy when using maps or folds etc.

    namedEmojiCodes=
      PairDict.empty
      |>PairDict.insert { left= 127824, right= "pear" }
      |>PairDict.insert { left= 127795, right= "tree" }
    namedEmojis=
      namedEmojiCodes
      |>PairDict.map (Pair.mapLeft String.fromInt)

> `PairDict.fromList [ ( '🍐', "pear" ), ( '🌳', "tree" ) ]`
-}
mapLeft:
  (left ->resultLeft)
  ->Pair left right
  ->Pair resultLeft right
mapLeft alter { left, right }=
  { left= alter left
  , right= right
  }

{-| map the `right` value in a `Pair`

    { left= "smile", right= 128522 }
    |>mapRight Char.fromCode

> `{ left= "smile", right= '😊' }`

This is handy when using maps or folds etc.

    namedEmojiCodes=
      PairDict.empty
      |>PairDict.insert { left= "pear", right= 127824 }
      |>PairDict.insert {  left= "tree", right= 127795 }
    namedEmojis=
      namedEmojiCodes
      |>PairDict.map (Pair.mapLeft String.fromInt)

> `PairDict.fromList [ ( "pear", '🍐' ), ( "tree", '🌳' ) ]`
-}
mapRight:
  (right ->resultRight)
  ->Pair left right
  ->Pair left resultRight
mapRight alter { left, right }=
  { left= left
  , right= alter right
  }


{-| Convert a `Pair` to a `Json.Encode.Value`.

    Encode.encode 1
      (Pair.encode
        Encode.int Encode.int
        { left= 1, right= 11 }
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
encode encodeLeft encodeRight { left, right }=
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


> `Ok { left= 1, right= 11 }`
-}
decode:
  Decoder left ->Decoder right
  ->Decoder (Pair left right)
decode decodeLeft decodeRight=
  Decode.map2 Pair
    (Decode.field "left" decodeLeft)
    (Decode.field "right" decodeRight)


