module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import PairDict exposing (empty)
import AssocList as AssocDict exposing (Dict)

import Json.Encode as Encode
import Json.Decode as Decode
import Pair


suite: Test
suite=
  let
    at0= { left= 0, right= 'A' }
    at1= { left= 1, right= 'B' }
    of2=
      empty
      |>PairDict.insert at0
      |>PairDict.insert at1
    listOf2=
      [ ( at0.left, at0.right )
      , ( at1.left, at1.right )
      ]
    brackets=
      PairDict.fromList
        [ ( '(', ')' )
        , ( '{', '}' )
        ]
  in
  describe "pair dict & pair"
    [ describe "pair"
        [ test "mapLeft example works"
          <|\()->
              Expect.equal
                ({ left= 128522, right= "smile" }
                |>Pair.mapLeft Char.fromCode
                )
                { left= '😊', right= "smile" }
        , test "mapRight example works"
          <|\()->
              Expect.equal
                ({ left= "smile", right= 128522 }
                |>Pair.mapRight Char.fromCode
                )
                { left= "smile", right= '😊' }
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
    , describe "pair dict"
        [ describe "create"
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
                          [ ( 'b', 'B' ) --+ { left= 'b', right= 'B' }
                          , ( 'a', 'A' ) --+ { left= 'a', right= 'A' }
                          , ( 'b', 'C' ) --ignored, as the left value already exists
                          , ( 'c', 'A' ) --ignored, as the right value already exists
                          , ( 'c', 'C' ) --+ { left= 'c', right= 'C' }
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
          
        , test "equal example works"
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
          
        , describe "access"
            [ test "finds left"
              <|\()->
                  Expect.equal
                    (Just at0.right) (PairDict.rightOf at0.left of2)
            , test "finds right"
              <|\()->
                  Expect.equal
                    (Just at1.left) (PairDict.leftOf at1.right of2)
            ]

        , describe "properties"
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
        
        , describe "in"
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
                      Expect.equal (Just at1.right)
                        (empty |>PairDict.insert at1
                        |>PairDict.rightOf at1.left 
                        )
                , test "rightOf left is Nothing if not of inserted pair"
                  <|\()->
                      Expect.equal Nothing
                        (empty |>PairDict.insert at1
                        |>PairDict.rightOf at0.left 
                        )
                , test "leftOf right is Just left of inserted pair"
                  <|\()->
                      Expect.equal (Just at1.left)
                        (empty |>PairDict.insert at1
                        |>PairDict.leftOf at1.right 
                        )
                , test "leftOf right is Nothing if not of inserted pair"
                  <|\()->
                      Expect.equal Nothing
                        (empty |>PairDict.insert at1
                        |>PairDict.leftOf at0.right 
                        )
                ]
            , test "union example"
              <|\()->
                  let
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
        
        , describe "out"
            [ test "add and remove left leaves it unchanged"
              <|\()->
                  let
                    insert= { left= 2, right= 'C' }
                  in
                  Expect.equal of2
                    (of2
                    |>PairDict.insert insert
                    |>PairDict.removeLeft insert.left
                    )
            , test "add and remove right leaves it unchanged"
              <|\()->
                  let
                    insert= { left= 2, right= 'C' }
                  in
                  Expect.equal of2
                    (of2
                    |>PairDict.insert insert
                    |>PairDict.removeRight insert.right
                    )
            ]
          
        , describe "shape"
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
                          (\{ left, right } acc->
                            acc ++[ String.fromList [ left, right ] ]
                          )
                          []
                  in
                  Expect.equal openingAndClosing [ "{}", "()" ]
            , test "map works as in the example"
              <|\()->
                  let
                    digitNames=
                      empty
                      |>PairDict.insert { left= 0, right= "zero" }
                      |>PairDict.insert { left= 1, right= "one" }
                    mathSymbolNames=
                      digitNames
                      |>PairDict.map
                          (Pair.mapLeft String.fromInt)
                      |>PairDict.insert { left= "+", right= "plus" }
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
            , describe "encode & decode"
                [ test "encoded & decoded PairDict is the same"
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
                ]
            ]
        
        , describe "readme examples work"
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
                      |>PairDict.insert { left= 'a', right= 'A' }
                      |>PairDict.insert { left= 'b', right= 'B' }
                      |>PairDict.insert { left= 'c', right= 'C' }

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
                      |>PairDict.insert { left= Hydrogen, right= 1 }
                      |>PairDict.insert { left= Helium, right= 2 }

                    protonsOfElements=
                      PairDict.toDict elementProtons
                  in
                  Expect.equal
                    [ Just 2, Just 1 ]
                    [ AssocDict.get Helium protonsOfElements
                    , AssocDict.get Hydrogen protonsOfElements
                    ]
            ]
        ]
    ]
type Element=
  Hydrogen
  | Helium
type alias ProtonCount= Int

