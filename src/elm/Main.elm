module Main exposing (..)

import Browser
import Html exposing (Html, a, div, h1, h2, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)


type Piece
    = Empty
    | Small Int String Color
    | Medium Int String Color
    | Large Int String Color


type Color
    = Red
    | Blue


type Turn
    = Player1
    | Player2
    | Player1Choosing Piece
    | Player2Choosing Piece


type RowPiece
    = First Piece
    | Second Piece
    | Third Piece


type Row
    = FirstRow RowPiece RowPiece RowPiece
    | SecondRow RowPiece RowPiece RowPiece
    | ThirdRow RowPiece RowPiece RowPiece


type alias Board =
    { first : Row
    , second : Row
    , third : Row
    }


type alias Player =
    { name : String
    , hand : List Piece
    }


type alias Model =
    { player1 : Player
    , player2 : Player
    , turn : Turn
    , board : Board
    }


type Msg
    = NoOp
    | SelectHandPiece Player Piece
    | SetPieceOnBoard Board Row RowPiece


removePiece : Piece -> List Piece -> List Piece
removePiece piece hand =
    List.filter (\handPiece -> id handPiece /= id piece) hand


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SelectHandPiece player piece ->
            let
                newHand =
                    removePiece piece player.hand

                newPlayer =
                    { player | hand = newHand }
            in
            case model.turn of
                Player1 ->
                    { model
                        | player1 = newPlayer
                        , turn = nextTurn model.turn piece
                    }

                Player2 ->
                    { model
                        | player2 = newPlayer
                        , turn = nextTurn model.turn piece
                    }

                _ ->
                    model

        SetPieceOnBoard board row rowPiece ->
            if model.turn == Player1 || model.turn == Player2 || not (isLegalMove rowPiece (pieceGoingOnBoard model.turn)) then
                model

            else
                let
                    pieceToPutOnBoard =
                        pieceGoingOnBoard model.turn

                    newRow =
                        case row of
                            FirstRow f s t ->
                                case rowPiece of
                                    First _ ->
                                        FirstRow (First pieceToPutOnBoard) s t

                                    Second _ ->
                                        FirstRow f (Second pieceToPutOnBoard) t

                                    Third _ ->
                                        FirstRow f s (Third pieceToPutOnBoard)

                            SecondRow f s t ->
                                case rowPiece of
                                    First _ ->
                                        SecondRow (First pieceToPutOnBoard) s t

                                    Second _ ->
                                        SecondRow f (Second pieceToPutOnBoard) t

                                    Third _ ->
                                        SecondRow f s (Third pieceToPutOnBoard)

                            ThirdRow f s t ->
                                case rowPiece of
                                    First _ ->
                                        ThirdRow (First pieceToPutOnBoard) s t

                                    Second _ ->
                                        ThirdRow f (Second pieceToPutOnBoard) t

                                    Third _ ->
                                        ThirdRow f s (Third pieceToPutOnBoard)

                    newBoard =
                        case newRow of
                            FirstRow _ _ _ ->
                                { board | first = newRow }

                            SecondRow _ _ _ ->
                                { board | second = newRow }

                            ThirdRow _ _ _ ->
                                { board | third = newRow }
                in
                { model
                    | board = newBoard
                    , turn = nextTurn model.turn Empty
                }


xIcon : String
xIcon =
    "❌"


oIcon : String
oIcon =
    "⭕"


init : Model
init =
    { player1 =
        { name = "Player 1"
        , hand = [ Small 1 "❌" Red, Small 2 "❌" Red, Medium 3 "❌" Red, Medium 4 "❌" Red, Large 5 "❌" Red, Large 6 "❌" Red ]
        }
    , player2 =
        { name = "Player 2"
        , hand = [ Small 1 "⭕" Blue, Small 2 "⭕" Blue, Medium 3 "⭕" Blue, Medium 4 "⭕" Blue, Large 5 "⭕" Blue, Large 6 "⭕" Blue ]
        }
    , turn = Player1
    , board =
        { first = FirstRow (First Empty) (Second Empty) (Third Empty)
        , second = SecondRow (First Empty) (Second Empty) (Third Empty)
        , third = ThirdRow (First Empty) (Second Empty) (Third Empty)
        }
    }


isLegalMove : RowPiece -> Piece -> Bool
isLegalMove rowPiece piece =
    let
        currentPiece =
            pieceFromRowPiece rowPiece
    in
    if currentPiece == Empty then
        True

    else
        isPieceSmaller currentPiece piece


isPieceSmaller : Piece -> Piece -> Bool
isPieceSmaller previous next =
    case previous of
        Small _ _ _ ->
            isMedium next || isLarge next

        Medium _ _ _ ->
            isLarge next

        Large _ _ _ ->
            False

        Empty ->
            True


isSmall : Piece -> Bool
isSmall piece =
    case piece of
        Small _ _ _ ->
            True

        _ ->
            False


isMedium : Piece -> Bool
isMedium piece =
    case piece of
        Medium _ _ _ ->
            True

        _ ->
            False


isLarge : Piece -> Bool
isLarge piece =
    case piece of
        Large _ _ _ ->
            True

        _ ->
            False


pieceGoingOnBoard : Turn -> Piece
pieceGoingOnBoard turn =
    case turn of
        Player1 ->
            Empty

        Player2 ->
            Empty

        Player1Choosing p ->
            p

        Player2Choosing p ->
            p


pieceFromRowPiece : RowPiece -> Piece
pieceFromRowPiece rowPiece =
    case rowPiece of
        First p ->
            p

        Second p ->
            p

        Third p ->
            p


id : Piece -> Int
id piece =
    case piece of
        Large id_ _ _ ->
            id_

        Medium id_ _ _ ->
            id_

        Small id_ _ _ ->
            id_

        Empty ->
            0


nextTurn : Turn -> Piece -> Turn
nextTurn turn piece =
    case turn of
        Player1 ->
            Player1Choosing piece

        Player2 ->
            Player2Choosing piece

        Player1Choosing _ ->
            Player2

        Player2Choosing _ ->
            Player1


pieceText : Piece -> String
pieceText piece =
    case piece of
        Large _ _ _ ->
            "large"

        Medium _ _ _ ->
            "medium"

        Small _ _ _ ->
            "small"

        Empty ->
            "empty"


pieceColorText : Piece -> String
pieceColorText piece =
    case piece of
        Large _ _ color ->
            colorText color

        Medium _ _ color ->
            colorText color

        Small _ _ color ->
            colorText color

        Empty ->
            ""


pieceLabelText : Piece -> String
pieceLabelText piece =
    case piece of
        Large _ label _ ->
            label

        Medium _ label _ ->
            label

        Small _ label _ ->
            label

        Empty ->
            ""


colorText : Color -> String
colorText color =
    case color of
        Red ->
            "red"

        Blue ->
            "blue"


rowPieces : Row -> List RowPiece
rowPieces row =
    case row of
        FirstRow f s t ->
            [ f, s, t ]

        SecondRow f s t ->
            [ f, s, t ]

        ThirdRow f s t ->
            [ f, s, t ]


pieceView : Piece -> Html Msg
pieceView piece =
    span
        [ class (pieceText piece)
        , class (pieceColorText piece)
        , class "piece"
        ]
        [ text (pieceLabelText piece) ]


playerHandView : Player -> Bool -> Html Msg
playerHandView player selectable =
    div [ class "player-hand" ]
        (List.map
            (\piece ->
                if selectable then
                    a [ href "#", onClick (SelectHandPiece player piece) ]
                        [ pieceView piece ]

                else
                    a [ href "#" ]
                        [ pieceView piece ]
            )
            player.hand
        )


playerView : Player -> Bool -> Html Msg
playerView player selectable =
    div [ class "player-view" ]
        [ div []
            [ h1 [] [ text player.name ] ]
        , div []
            [ playerHandView player selectable ]
        ]


currentTurnView : Turn -> Html Msg
currentTurnView turn =
    let
        turnText =
            case turn of
                Player1 ->
                    "Player 1's Turn"

                Player2 ->
                    "Player 2's Turn"

                Player1Choosing _ ->
                    "Player 1's Turn"

                Player2Choosing _ ->
                    "Player 2's Turn"

        piece =
            case turn of
                Player1 ->
                    Empty

                Player2 ->
                    Empty

                Player1Choosing piece_ ->
                    piece_

                Player2Choosing piece_ ->
                    piece_
    in
    div [ class "current-turn" ]
        [ h2 []
            [ text turnText
            , pieceView piece
            ]
        ]


boardRowPieceView : Board -> Row -> RowPiece -> Html Msg
boardRowPieceView board row rowPiece =
    a [ class "row-piece", href "#", onClick (SetPieceOnBoard board row rowPiece) ]
        [ case rowPiece of
            First piece ->
                pieceView piece

            Second piece ->
                pieceView piece

            Third piece ->
                pieceView piece
        ]


boardRowView : Board -> Row -> Html Msg
boardRowView board row =
    div [ class "row" ]
        (List.map (boardRowPieceView board row) (rowPieces row))


boardView : Model -> Html Msg
boardView model =
    div [ class "board" ]
        [ boardRowView model.board model.board.first
        , boardRowView model.board model.board.second
        , boardRowView model.board model.board.third
        ]


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ div [ class "board-area" ]
            [ currentTurnView model.turn
            , boardView model
            ]
        , div []
            [ playerView model.player1 (model.turn == Player1)
            , playerView model.player2 (model.turn == Player2)
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
