module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import PairDict exposing (PairDict, empty)
import AssocList as AssocDict exposing (Dict)

import Json.Encode as Encode
import Json.Decode as Decode
import Pair exposing (Pair)


suite: Test
suite=
  describe "pair dict & pair"
    [ pairTest
    , describe "pair dict"
        [ pairDictCreateTest
        , pairDictEqualTest
        , pairDictAccessTest
        , pairDictPropertyTest
        , pairDictInTest
        , pairDictOutTest
        , pairDictShapeTest
        , readmeExamplesTest
        ]
    ]

at0: Pair Int Char
at0= ( 0, 'A' )

at1: Pair Int Char
at1= ( 1, 'B' )

pairTest: Test
pairTest=
  describe "pair"
    [ describe "access"
        [ test "leftIn returns the left value"
          <|\()->
              Pair.leftIn ( 'a', "not a" )
              |>Expect.equal 'a'
        , test "rightIn returns the left value"
          <|\()->
              Pair.rightIn ( "not z", 'z' )
              |>Expect.equal 'z'
        ]
    , test "mapLeft example works"
      <|\()->
          Expect.equal
            (( 128522, "smile" )
            |>Pair.mapLeft Char.fromCode
            )
            ( '😊', "smile" )
    , test "mapRight example works"
      <|\()->
          Expect.equal
            (( "smile", 128522 )
            |>Pair.mapRight Char.fromCode
            )
            ( "smile", '😊' )
    , test "encoded & decoded Pair is the same"
      <|\()->
          let
            encodedDecoded=
              at0
              |>Pair.encode
                  Encode.int
                  (Char.toCode >>Encode.int)
              |>Decode.decodeValue
                  (Pair.decode
                    Decode.int
                    (Decode.map Char.fromCode Decode.int)
                  )
          in
          case encodedDecoded of
            Ok decoded->
              Expect.equal decoded at0
            
            Err err->
              Expect.fail (Decode.errorToString err)
    ]


of2: PairDict Int Char
of2=
  empty
  |>PairDict.insert at0
  |>PairDict.insert at1

listOf2: List (Pair Int Char)
listOf2=
  [ at0, at1 ]

brackets: PairDict.PairDict Char Char
brackets=
  PairDict.fromList
    [ ( '(', ')' )
    , ( '{', '}' )
    ]

pairDictCreateTest: Test
pairDictCreateTest=
  describe "create"
    [ describe "fromList is the same as empty |>inserting"
        [ test "empty  is  fromList []"
          <|\()->
              Expect.equal empty (PairDict.fromList [])
        , test "fromList gives same result as inserting"
          <|\()->
              Expect.true "fromList gives same result as inserting"
                (PairDict.equal
                  of2 (PairDict.fromList listOf2)
                )
        , test "fromList ignores duplicates as in example"
          <|\()->
              let
                badList=
                  [ ( 'b', 'B' ) --+ ( 'b', 'B' )
                  , ( 'a', 'A' ) --+ ( 'a', 'A' )
                  , ( 'b', 'C' ) --ignored, as the left value already exists
                  , ( 'c', 'A' ) --ignored, as the right value already exists
                  , ( 'c', 'C' ) --+ ( 'c', 'C' )
                  ]
              in
              Expect.equal 3
                (PairDict.size (PairDict.fromList badList))
        , test "fromDict dictFromLeft returns an equal dict"
          <|\()->
              let
                lowerToUpperLetters=
                  AssocDict.empty
                  |>AssocDict.insert 'a' 'A'
                  |>AssocDict.insert 'b' 'B'

                lowerUpperLetters=
                  PairDict.fromDict lowerToUpperLetters
              in
              Expect.true "expected fromDict toDict returns an equal dict"
                (AssocDict.eq
                  lowerToUpperLetters
                  (PairDict.toDict lowerUpperLetters)
                )
        ]
    ]

pairDictEqualTest: Test
pairDictEqualTest=
  test "equal example works"
  <|\()->
      let
        letterCodes=
          PairDict.fromList
            [ ( 'a', 0 ), ( 'b', 1 ) ]
        fancyCompetingLetterCodes=
          PairDict.fromList
            [ ( 'b', 1 ), ( 'a', 0 ) ]
      in
      Expect.true "reversed list fromList equal to fromList"
        (PairDict.equal
          letterCodes
          fancyCompetingLetterCodes
        )

pairDictAccessTest: Test
pairDictAccessTest=
  describe "access"
    [ test "finds left"
      <|\()->
          PairDict.rightOf (Pair.leftIn at0) of2
          |>Expect.equal
              (Just (Pair.rightIn at0))
    , test "finds right"
      <|\()->
          PairDict.leftOf (Pair.rightIn at1) of2
          |>Expect.equal
              (Just (Pair.leftIn at1))
    ]


pairDictPropertyTest: Test
pairDictPropertyTest=
  describe "property"
    [ describe "size, as in the examples"
        [ test "size of empty is 0"
          <|\()->
              Expect.equal (PairDict.size empty) 0
        , test "dict has the same size as a list of unique values"
          <|\()->
              Expect.equal 42
                (PairDict.size
                  (PairDict.fromList
                    (List.range 0 41
                    |>List.map (\i-> ( i, i ))
                    )
                  )
                )
        ]
    ]

pairDictInTest: Test
pairDictInTest=
  describe "in"
    [ describe "insert"
        [ test "insert is ignored for duplicates"
          <|\()->
              Expect.equal 2
                (PairDict.size
                  (of2
                  |>PairDict.insert at0
                  |>PairDict.insert at1
                  )
                )
        , test "rightOf left is Just right of inserted pair"
          <|\()->
              Expect.equal (Just (Pair.rightIn at1))
                (empty |>PairDict.insert at1
                |>PairDict.rightOf (Pair.leftIn at1) 
                )
        , test "rightOf left is Nothing if not of inserted pair"
          <|\()->
              Expect.equal Nothing
                (empty |>PairDict.insert at1
                |>PairDict.rightOf (Pair.leftIn at0) 
                )
        , test "leftOf right is Just left of inserted pair"
          <|\()->
              Expect.equal (Just (Pair.leftIn at1))
                (empty |>PairDict.insert at1
                |>PairDict.leftOf (Pair.rightIn at1)
                )
        , test "leftOf right is Nothing if not of inserted pair"
          <|\()->
              Expect.equal Nothing
                (empty |>PairDict.insert at1
                |>PairDict.leftOf ( Pair.rightIn at0)
                )
        ]
    , let
        numberNamedOperators=
          [ ( '+', "plus" )
          , ( '-', "minus" )
          ]
        custumNamedOperators=
          [ ( '∧', "and" )
          , ( '∨', "or" )
          , ( '-', "negate" )
          ]
        validNamedOperators=
          PairDict.union
            (PairDict.fromList custumNamedOperators)
            (PairDict.fromList numberNamedOperators)
        fromListOfConcatenated=
          PairDict.fromList
            (custumNamedOperators
            ++numberNamedOperators
            )
        encode=
          PairDict.encode
            (Encode.string <<String.fromChar)
            Encode.string
          >>Encode.encode 2
      in
      test "union example"
      <|\()->
          Expect.true
            ("union of fromList dicts ("
            ++(encode validNamedOperators)
            ++"equal to fromList of concatenated lists ("
            ++(encode fromListOfConcatenated)
            ++")"
            )
            (PairDict.equal
              validNamedOperators
              fromListOfConcatenated
            )
    ]

pairDictOutTest: Test
pairDictOutTest=
  describe "out"
    [ test "add and remove left leaves it unchanged"
      <|\()->
          of2
          |>PairDict.insert ( 2, 'C' )
          |>PairDict.removeLeft 2
          |>Expect.equal of2
    , test "add and remove right leaves it unchanged"
      <|\()->
          of2
          |>PairDict.insert ( 2, 'C' )
          |>PairDict.removeRight 'C'
          |>Expect.equal of2
    ]


pairDictShapeTest: Test
pairDictShapeTest=
  describe "shape"
    [ test "lefts are the same as of the list"
      <|\()->
          Expect.equal
            (of2 |>PairDict.lefts)
            (listOf2 |>List.reverse |>List.map Tuple.first)
    , test "rights are the same as of the list"
      <|\()->
          Expect.equal
            (of2 |>PairDict.rights)
            (listOf2 |>List.reverse |>List.map Tuple.second)
    , test "fold works as in the example"
      <|\()->
          let
            openingAndClosing=
              brackets
              |>PairDict.fold
                  (\( left, right ) acc->
                    acc ++[ String.fromList [ left, right ] ]
                  )
                  []
          in
          Expect.equal
            openingAndClosing [ "{}", "()" ]
    , test "map works as in the example"
      <|\()->
          let
            digitNames=
              empty
              |>PairDict.insert ( 0, "zero" )
              |>PairDict.insert ( 1, "one" )
            mathSymbolNames=
              digitNames
              |>PairDict.map
                  (Pair.mapLeft String.fromInt)
              |>PairDict.insert ( "+", "plus" )
          in
          Expect.true "mapped PairDict equal to fromList"
            (PairDict.equal
              mathSymbolNames
              (PairDict.fromList
                [ ( "0", "zero" )
                , ( "1", "one" )
                , ( "+", "plus" )
                ]
              )
            )
    , test "toDict example works"
      <|\()->
          let
            casedLetterList=
              [ ( 'A', 'a' ), ( 'B', 'b' ) ]
            casedLetters=
              PairDict.fromList casedLetterList
            lowerFromUpper=
              casedLetters |>PairDict.toDict
          in
          Expect.true "PairDict.fromList toDict equal to AssocDict.fromList"
            (AssocDict.eq
              lowerFromUpper
              (AssocDict.fromList casedLetterList)
            )
    , pairDictEncodeDecodeTest
    ]

pairDictEncodeDecodeTest: Test
pairDictEncodeDecodeTest=
  test "encoded & decoded PairDict is the same"
  <|\()->
      let
        encoded=
          of2
          |>PairDict.encode
              Encode.int
              (Char.toCode >>Encode.int)
        encodedDecoded=
          encoded
          |>Decode.decodeValue
              (PairDict.decode
                Decode.int
                (Decode.map Char.fromCode Decode.int)
              )
      in
      case encodedDecoded of
        Ok decoded->
          Expect.true "encoded |>decoded equal to before"
            (PairDict.equal decoded of2)
        
        Err err->
          Expect.fail (Decode.errorToString err)


type Element=
  Hydrogen
  | Helium
type alias ProtonCount= Int

readmeExamplesTest: Test
readmeExamplesTest=
  describe "readme examples work"
    [ test "braces"
      <|\()->
          let
            typeChar character=
              brackets
              |>PairDict.leftOf character
              |>Maybe.map
                  (\opening->
                    String.fromList [ opening, character ]
                  )
              |>Maybe.withDefault
                  (brackets
                  |>PairDict.rightOf character
                  |>Maybe.map
                      (\closing->
                        String.fromList [ character, closing ]
                      )
                  |>Maybe.withDefault
                      (String.fromChar character)
                  )
            in
            Expect.equal
              ([ '(', '}' ] |>List.map typeChar)
              [ "()", "{}" ]
    , test "cased letters"
      <|\()->
          let
            casedLetters=
              empty
              |>PairDict.insert ( 'a', 'A' )
              |>PairDict.insert ( 'b', 'B' )
              |>PairDict.insert ( 'c', 'C' )

            upperCase char=
              PairDict.rightOf char casedLetters
          in
          Expect.equal
            ([ 'c', 'a', 'x' ] |>List.map upperCase)
            ([ Just 'C', Just 'A', Nothing ])
    , test "periodic table"
      <|\()->
          let
            elementProtons=
              empty
              |>PairDict.insert ( Hydrogen, 1 )
              |>PairDict.insert ( Helium, 2 )

            protonsOfElements=
              PairDict.toDict elementProtons
          in
          Expect.equal
            [ Just 2, Just 1 ]
            [ AssocDict.get Helium protonsOfElements
            , AssocDict.get Hydrogen protonsOfElements
            ]
    ]

