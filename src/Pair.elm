module Pair exposing
  ( Pair
  , encode, decode
  )
{-|
@docs Pair

## create
through the exposed constructor `{ left, right }` or
@docs decode

## shape
@docs encode
-}

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


{-| `left` & `right` as a value-pair
To create, use

    { left= {-leftValue-}, right= {-rightValue-} }
-}
type alias Pair left right=
  { left: left
  , right: right
  }

{-| Convert a `Pair` to a `Json.Encode.Value`.

    Encode.encode 1
      (Pair.encode
        Encode.int Encode.int
        { left= 1, right= 11 }
      )
    {->
    """
    {
     \"left\": 1,
     \"right\": 11
    }
    """
    -}
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
    --> Ok { left= 1, right= 11 }
-}
decode:
  Decoder left ->Decoder right
  ->Decoder (Pair left right)
decode decodeLeft decodeRight=
  Decode.map2 Pair
    (Decode.field "left" decodeLeft)
    (Decode.field "right" decodeRight)


