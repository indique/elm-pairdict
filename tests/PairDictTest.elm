module PairDictTest exposing (..)


import Test exposing (Test, test, describe)
import Expect

import PairDict exposing (PairDict, empty)
import AssocList as AssocDict exposing (Dict)

import Json.Encode as Encode
import Json.Decode as Decode


type alias CharWithCode=
  { code: Int, char: Char }

at0: CharWithCode
at0= { code= 0, char= 'A' }

at1: CharWithCode
at1= { code= 1, char= 'B' }


of2: PairDict CharWithCode Int Char
of2=
  empty .code .char
  |>PairDict.putIn at0
  |>PairDict.putIn at1

listOf2: List CharWithCode
listOf2=
  [ at0, at1 ]

type alias BracketMatch=
  { open: Char, closed: Char }

brackets: PairDict BracketMatch Char Char
brackets=
  PairDict.fromList .open .closed
    [ { open= '(', closed= ')' }
    , { open= '{', closed= '}' }
    ]

type alias CasedLetter=
  { lowercase: Char
  , uppercase: Char
  }

createPairDictTest: Test
createPairDictTest=
  describe "create"
    [ describe "fromList is the same as empty |>inserting"
        [ test "empty  is  fromList []"
          <|\()->
              PairDict.equal
                (empty .code .char)
                (PairDict.fromList .code .char [])
              |>Expect.true "empty equal to fromList []"
        , test "fromList gives same result as inserting"
          <|\()->
              PairDict.equal
                of2
                (PairDict.fromList .code .char listOf2)
              |>Expect.true "fromList gives same result as inserting"
        , test "fromList ignores duplicates as in example"
          <|\()->
              let
                badList=
                  [ { lowercase= 'b', uppercase= 'B' } --put in
                  , { lowercase= 'a', uppercase= 'A' } --put in
                  , { lowercase= 'b', uppercase= 'C' } --ignored, as the left value already exists
                  , { lowercase= 'c', uppercase= 'A' } --ignored, as the right value already exists
                  , { lowercase= 'c', uppercase= 'C' } --put in
                  ]
              in
              PairDict.size
                (PairDict.fromList .lowercase .uppercase badList)
              |>Expect.equal 3
        , test "fromDict dictFromLeft returns an equal dict"
          <|\()->
              let
                lowerToUpperLetters=
                  AssocDict.empty
                  |>AssocDict.insert 'a' 'A'
                  |>AssocDict.insert 'b' 'B'

                lowerUpperLetters=
                  PairDict.fromDict
                    (\k v-> { lowercase= k, uppercase= v })
                    .lowercase .uppercase
                    lowerToUpperLetters
              in
              Expect.true "expected fromDict toDict returns an equal dict"
                (AssocDict.eq
                  lowerToUpperLetters
                  (PairDict.toDict lowerUpperLetters)
                )
        ]
    ]

scanPairDictScanTest: Test
scanPairDictScanTest=
  describe "scan"
    [ describe "size"
        [ test "size of empty is 0"
          <|\()->
              PairDict.size (empty .code .char)
              |>Expect.equal 0
        , test "dict has the same size as a list of unique values"
          <|\()->
              Expect.equal 42
                (PairDict.size
                  (PairDict.fromList
                    Tuple.first Tuple.second
                    (List.range 0 41
                    |>List.map (\i-> ( i, i ))
                    )
                  )
                )
        ]
    , describe "access"
        (let
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
        in
        [ test "finds lowercase"
          <|\()->
              List.map lowercase [ 'a', 'B' ]
              |>Expect.equal
                  [ Nothing, Just 'b' ]
        , test "finds uppercase"
          <|\()->
              List.map uppercase [ 'b', 'A' ]
              |>Expect.equal
                  [ Just 'B', Nothing ]
        ]
        )
    , test "equal example works"
      <|\()->
          let
            letterCodes=
              PairDict.fromList .letter .code 
                [ { letter= 'a', code= 97 }
                , { letter= 'b', code= 98 }
                ]
            fancyCompetingLetterCodes=
              PairDict.empty .code .letter
              |>PairDict.putIn { code= 98, letter= 'b' }
              |>PairDict.putIn { code= 97, letter= 'a' }
          in
          PairDict.equal
            letterCodes
            fancyCompetingLetterCodes
          |>Expect.true "reversed list with switched left right fromList equal to fromList"
    , describe "emptyOrMore examples work"
        [ test "isEmpty"
          <|\()->
            let
              isEmpty=
                PairDict.emptyOrMore
                  { ifEmpty= True
                  , ifMore= \_ _-> False
                  }
            in
            Expect.true "isEmpty for filled False, ifEmpty True"
              ((&&)
                (isEmpty (empty .code .char))
                (not (isEmpty of2))
              )
        , test "most recently inserted"
          <|\()->
            let
              mostRecentlyInserted=
                PairDict.emptyOrMore
                  { ifMore= \pair _-> Just pair
                  , ifEmpty= Nothing
                  }
            in
            mostRecentlyInserted
              (PairDict.fromList .lowercase .uppercase
                [ { lowercase= 'a', uppercase= 'A' },
                  { lowercase= 'b', uppercase= 'B' }
                ]
              )
            |>Expect.equal
                (Just { lowercase= 'b', uppercase= 'B' })
        ]
    ]

inPairDictTest: Test
inPairDictTest=
  describe "in"
    [ describe "insert"
        [ test "insert is ignored for duplicates"
          <|\()->
              PairDict.size
                (of2
                |>PairDict.putIn at0
                |>PairDict.putIn at1
                )
              |>Expect.equal 2
        , test "access code is Just letter of inserted pair"
          <|\()->
              empty .code .char
              |>PairDict.putIn at1
              |>PairDict.access .code (.code at1)
              |>Maybe.map .char
                |>Expect.equal (Just (.char at1))
        , test "access code is Nothing if not of inserted pair"
          <|\()->
              empty .code .char
              |>PairDict.putIn at1
              |>PairDict.access .code (.code at0)
              |>Maybe.map .char
                |>Expect.equal Nothing
        , test "access char is Just left of inserted pair"
          <|\()->
              empty .code .char
              |>PairDict.putIn at1
              |>PairDict.access .char (.char at1)
              |>Maybe.map .code
                |>Expect.equal (Just (.code at1))
        , test "access char is Nothing if not of inserted pair"
          <|\()->
              empty .code .char
              |>PairDict.putIn at1
              |>PairDict.access .char (.char at0)
              |>Maybe.map .code
                |>Expect.equal Nothing
        ]
    , let
        numberNamedOperators=
          [ { operator= '+', name= "plus" }
          , { operator= '-', name= "minus" }
          ]
        custumNamedOperators=
          [ { operator= '∧', name= "and" }
          , { operator= '∨', name= "or" }
          , { operator= '-', name= "negate" }
          ]
        validNamedOperators=
          PairDict.union
            (PairDict.fromList .operator .name
              custumNamedOperators
            )
            (PairDict.fromList .operator .name
              numberNamedOperators
            )
        fromListOfConcatenated=
          PairDict.fromList .operator .name
            (numberNamedOperators
            ++custumNamedOperators
            )
        encode=
          PairDict.encode
            (\{ operator, name }->
              Encode.object
                [ ( "operator"
                  , Encode.string (String.fromChar operator)
                  )
                , ( "name", Encode.string name )
                ]
            )
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

outPairDictTest: Test
outPairDictTest=
  describe "out"
    [ test "add and remove code leaves it unchanged"
      <|\()->
          of2
          |>PairDict.putIn { code= 2, char= 'C' }
          |>PairDict.remove .code 2
          |>Expect.equal of2
    , test "add and remove char leaves it unchanged"
      <|\()->
          of2
          |>PairDict.putIn { code= 2, char= 'C' }
          |>PairDict.remove .char 'C'
          |>Expect.equal of2
    ]


shapePairDictTest: Test
shapePairDictTest=
  describe "shape"
    [ test "codes are the same as of the list"
      <|\()->
          Expect.equal
            (of2 |>PairDict.values .code)
            (listOf2 |>List.reverse |>List.map .code)
    , test "chars are the same as of the list"
      <|\()->
          Expect.equal
            (of2 |>PairDict.values .char)
            (listOf2 |>List.reverse |>List.map .char)
    , test "fold works as in the example"
      <|\()->
          let
            openingAndClosing=
              brackets
              |>PairDict.fold
                  (\{ open, closed } acc->
                    acc ++[ String.fromList [ open, closed ] ]
                  )
                  []
          in
          Expect.equal
            openingAndClosing [ "{}", "()" ]
    , test "map works as in the example"
      <|\()->
          let
            digitNames=
              PairDict.empty .number .name
              |>PairDict.putIn { number= 0, name= "zero" }
              |>PairDict.putIn { number= 1, name= "one" }

            mathSymbolNames=
              digitNames
              |>PairDict.map
                  (\{ number, name }->
                    { symbol= String.fromInt number, name= name }
                  )
                  .symbol .name
              |>PairDict.putIn { symbol= "+", name= "plus" }
          in
          Expect.true "mapped PairDict equal to fromList"
            (PairDict.equal
              mathSymbolNames
              (PairDict.fromList .symbol .name
                [ { symbol= "0", name= "zero" }
                , { symbol= "1", name= "one" }
                , { symbol= "+", name= "plus" }
                ]
              )
            )
    , test "toDict example works"
      <|\()->
          let
            casedLetters=
              PairDict.fromList .lowercase .uppercase
                [ { uppercase= 'A', lowercase= 'a' }
                , { uppercase= 'B', lowercase= 'b' }
                ]
            lowerFromUpper=
              PairDict.toDict casedLetters
          in
          Expect.true "PairDict.fromList toDict equal to AssocDict.fromList"
            (AssocDict.eq
              lowerFromUpper
              (AssocDict.fromList [ ( 'a', 'A' ), ( 'b', 'B' ) ])
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
              (\{ code, char }->
                Encode.object
                  [ ( "code", Encode.int code )
                  , ( "char", Encode.int (Char.toCode char) )
                  ]
              )
        encodedDecoded=
          encoded
          |>Decode.decodeValue
              (PairDict.decode .code .char
                (Decode.map2 CharWithCode
                  (Decode.field "code" Decode.int)
                  (Decode.field "char"
                    (Decode.map Char.fromCode Decode.int)
                  )
                )
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
              |>PairDict.access .open character
              |>Maybe.map
                  (\{ closed }->
                    String.fromList [ character, closed ]
                  )
              |>Maybe.withDefault
                  (brackets
                  |>PairDict.access .closed character
                  |>Maybe.map
                      (\{ open }->
                        String.fromList [ open, character ]
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
            lowerUppercaseLetters: PairDict CasedLetter Char Char
            lowerUppercaseLetters=
              PairDict.empty .lowercase .uppercase
              |>PairDict.putIn { lowercase= 'a', uppercase= 'A' }
              |>PairDict.putIn { lowercase= 'b', uppercase= 'B' }
              |>PairDict.putIn { lowercase= 'c', uppercase= 'C' }

            upperCase char=
              PairDict.access .lowercase char lowerUppercaseLetters
              |>Maybe.map .uppercase
          in
          Expect.equal
            ([ 'c', 'a', 'x' ] |>List.map upperCase)
            ([ Just 'C', Just 'A', Nothing ])
    , test "periodic table"
      <|\()->
          let
            elementAtomicNumberPairdict=
              PairDict.fromList .element .atomicNumber
                [ { element= Hydrogen, atomicNumber= 1 }
                , { element= Helium, atomicNumber= 2 }
                ]
            atomicNumberByElement=
              PairDict.toDict
                elementAtomicNumberPairdict
          in
          Expect.equal
            [ Just 2, Just 1 ]
            [ AssocDict.get Helium atomicNumberByElement
            , AssocDict.get Hydrogen atomicNumberByElement
            ]
    ]

