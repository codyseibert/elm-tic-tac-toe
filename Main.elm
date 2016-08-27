module Main exposing (..)

import Html exposing (div, button, text)
import Html.Attributes exposing (..)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import List
import Random
import Set
import Array
import Debug


main =
    beginnerProgram { model = initialModel, view = view, update = update }


type Msg
    = CellMouseEnter Cell
    | CellMouseLeave Cell
    | CellClick Cell
    | NewGame


type Turn
    = Computer
    | Player


type State
    = X
    | O
    | C
    | Blank


type alias Cell =
    { i : Int
    , j : Int
    , state : State
    , hovered : Bool
    }


type alias Board =
    List Cell


type alias Model =
    { board : Board
    , turn : Turn
    , isGameOver : Bool
    , winner : State
    , seed : Random.Seed
    }


updateBoard newCell cells =
    cells
        |> List.map
            (\c ->
                if isSameCell newCell c then
                    newCell
                else
                    c
            )


isSameCell a b =
    a.i == b.i && a.j == b.j


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


createBoard : Board
createBoard =
    cartesian [0..2] [0..2]
        |> List.map
            (\( i, j ) ->
                { i = i
                , j = j
                , state = Blank
                , hovered = False
                }
            )


initialModel : Model
initialModel =
    { board = createBoard
    , turn = Player
    , isGameOver = False
    , winner = Blank
    , seed = Random.initialSeed 31415
    }


isBoardComplete board =
    0 == (board |> List.filter (\c -> c.state == Blank) |> List.length)


getOpenCell board seed =
    let
        openCells =
            board
                |> List.filter (\c -> c.state == Blank)

        gen =
            Random.int 0 (List.length openCells)

        ( rnd, seed' ) =
            Random.step gen seed

        randomCell =
            openCells |> Array.fromList |> Array.get rnd
    in
        ( randomCell, seed' )


getWinner board =
    let
        winningDirections =
            [ [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]
            , [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
            , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
            , [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
            , [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
            , [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]
            , [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
            , [ ( 2, 0 ), ( 1, 1 ), ( 0, 2 ) ]
            ]

        playerCells =
            board |> List.filter (\c -> c.state == X) |> List.map (\c -> ( c.i, c.j ))

        computerCells =
            board |> List.filter (\c -> c.state == O) |> List.map (\c -> ( c.i, c.j ))

        isWinner cells =
            winningDirections
                |> List.map
                    (\direction ->
                        let
                            a =
                                direction |> Set.fromList

                            b =
                                cells |> Set.fromList

                            intersection =
                                Set.intersect a b
                        in
                            intersection |> Set.toList |> List.length
                    )
                |> List.any (\len -> len == 3)
    in
        if isWinner playerCells then
            X
        else if isWinner computerCells then
            O
        else
            Blank


placeComputerPiece : Board -> Random.Seed -> ( List Cell, Random.Seed )
placeComputerPiece board seed =
    let
        ( openCell, seed' ) =
            getOpenCell board seed
    in
        case openCell of
            Nothing ->
                ( board, seed' )

            Just cell ->
                ( updateBoard { cell | state = O } board, seed' )


update : Msg -> Model -> Model
update msg model =
    case msg of
        CellMouseEnter cell ->
            if model.isGameOver == True || cell.state /= Blank then
                model
            else
                { model | board = updateBoard { cell | hovered = True } model.board }

        CellMouseLeave cell ->
            { model | board = updateBoard { cell | hovered = False } model.board }

        NewGame ->
            initialModel

        CellClick cell ->
            if model.turn /= Player || model.isGameOver == True || cell.state /= Blank then
                model
            else
                let
                    newModelState =
                        { model | turn = Computer, board = updateBoard { cell | state = X, hovered = False } model.board }

                    winner =
                        getWinner newModelState.board
                in
                    if winner /= Blank then
                        { newModelState | isGameOver = True, winner = winner }
                    else
                        let
                            ( boardAfterPlace, seed' ) =
                                placeComputerPiece newModelState.board newModelState.seed

                            afterComputerState =
                                { model | seed = seed', board = boardAfterPlace, turn = Player }

                            winner =
                                getWinner afterComputerState.board
                        in
                            if isBoardComplete afterComputerState.board then
                                { afterComputerState | isGameOver = True, winner = C }
                            else if winner /= Blank then
                                { afterComputerState | isGameOver = True, winner = winner }
                            else
                                afterComputerState


getPosition : Int -> Int -> Int -> ( Int, Int )
getPosition i j w =
    ( i * w, j * w )


getCellStyle cell =
    let
        dim =
            100

        ( y, x ) =
            getPosition cell.i cell.j dim
    in
        style
            [ ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "position", "absolute" )
            , ( "width", toString dim ++ "px" )
            , ( "height", toString dim ++ "px" )
            , ( "text-align", "center" )
            , ( "border", "1px solid black" )
            , (if cell.state == Blank then
                ( "cursor", "pointer" )
               else
                ( "", "" )
              )
            , ( "line-height", toString dim ++ "px" )
            , if cell.hovered then
                ( "background-color", "orange" )
              else
                ( "background-color", "rgba(0, 255, 0, 0.5)" )
            ]


view model =
    div
        [ style
            [ ( "position", "relative" )
            ]
        ]
        [ div
            [ style
                [ ( "font-size", "40px" )
                , ( "text-align", "center" )
                , ( "margin-top", "60px" )
                ]
            ]
            [ text
                (if model.winner == C then
                    "Cats game!"
                 else if model.winner /= Blank then
                    toString model.winner ++ " has won the game!"
                 else if model.turn == Player then
                    "It is your turn!  Please click on a cell."
                 else
                    "It is the computer's turn!"
                )
            , div []
                [ button
                    [ onClick NewGame
                    , style
                        (if model.isGameOver == False then
                            [ ( "display", "none" ) ]
                         else
                            []
                        )
                    ]
                    [ text "Start New Game" ]
                ]
            ]
        , div
            [ style
                [ ( "position", "relative" )
                , ( "margin", "0 auto" )
                , ( "width", "300px" )
                , ( "margin-top", "100px" )
                ]
            ]
            (model.board
                |> List.map
                    (\cell ->
                        let
                            cellText =
                                case cell.state of
                                    X ->
                                        "X"

                                    O ->
                                        "O"

                                    Blank ->
                                        ""

                                    C ->
                                        ""

                            cellStyle =
                                getCellStyle cell
                        in
                            div
                                [ cellStyle
                                , onMouseEnter (CellMouseEnter cell)
                                , onMouseLeave (CellMouseLeave cell)
                                , onClick (CellClick cell)
                                ]
                                [ cellText |> text ]
                    )
            )
        ]
