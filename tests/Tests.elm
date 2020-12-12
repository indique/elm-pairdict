module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import PairDict exposing (empty)
import Dict


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
  describe "two-way dict"
    [ describe "create"
        [ describe "fromList is the same as inserting"
            [ test "empty  is  fromList []"
              <|\()->
                  Expect.equal empty (PairDict.fromList [])
            , test "fromList gives same result as inserting"
              <|\()->
                  Expect.equal
                    of2 (PairDict.fromList listOf2)
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
            , test "fromDict toDict returns an equal dict"
              <|\()->
                  let
                    lowerToUpperLetters=
                      Dict.empty
                      |>Dict.insert 'a' 'A'
                      |>Dict.insert 'b' 'B'

                    lowerUpperLetters= PairDict.fromDict lowerToUpperLetters
                  in
                  Expect.equalDicts
                    lowerToUpperLetters
                    (PairDict.toDict lowerUpperLetters)
            ]
        ]
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
        [ test "union example"
          <|\()->
              let
                numberOperatorNames=
                    [ ( '+', "plus" )
                    , ( '-', "minus" )
                    ]
                boolOperatorNames=
                    [ ( '∧', "and" )
                    , ( '∨', "or" )
                    ]
                operatorNames=
                  PairDict.union
                    (PairDict.fromList numberOperatorNames)
                    (PairDict.fromList boolOperatorNames)
              in
              Expect.equal
                operatorNames
                (PairDict.fromList
                  (numberOperatorNames ++boolOperatorNames)
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
    , describe "transform"
        [ test "lefts are the same as of the list"
          <|\()->
              Expect.equal
                (of2 |>PairDict.lefts)
                (listOf2 |>List.map Tuple.first)
        , test "rights are the same as of the list"
          <|\()->
              Expect.equal
                (of2 |>PairDict.rights)
                (listOf2 |>List.map Tuple.second)
        , describe "swap left-right"
            [ test "swapping two times is the same as the original"
                <|\()->
                    Expect.equal
                      of2 (PairDict.swapLeftRight<|PairDict.swapLeftRight of2)
            , test "swap equal to fromList"
              <|\()->
                  Expect.equal
                    (PairDict.swapLeftRight of2)
                    (PairDict.fromList
                      (listOf2
                      |>List.map (\( left, right )-> ( right, left ))
                      )
                    )
            ]
        , test "foldl works as in the example"
          <|\()->
              let
                openingAndClosing=
                  brackets
                  |>PairDict.foldl
                      (\{ left, right } acc->
                        acc ++[ String.fromList [ left, right ] ]
                      )
                      []
              in
              Expect.equal openingAndClosing [ "()", "{}" ]
        , test "foldr works as in the example"
          <|\()->
              let
                openingAndClosing=
                  brackets
                  |>PairDict.foldr
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
                      (\{left,right}-> { left= String.fromInt left, right= right })
                  |>PairDict.insert { left= "+", right= "plus" }
              in
              Expect.equal
                mathSymbolNames
                (PairDict.fromList
                  [ ( "0", "zero" )
                  , ( "1", "one" )
                  , ( "+", "plus" )
                  ]
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
              Expect.equal lowerFromUpper
                (Dict.fromList casedLetterList)
        ]
    , describe "concrete examples work"
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
                  ("Typing (: " ++(typeChar '(') ++". Even }: " ++(typeChar '}'))
                  "Typing (: (). Even }: {}"
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
                (upperCase 'c') (Just 'C')
        ]
    ]
