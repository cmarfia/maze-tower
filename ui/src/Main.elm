module Main exposing (main)

import Api
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Maze exposing (Maze)
import Msg exposing (Msg(..))


type GameState
    = Ready
    | Starting
    | Active Maze
    | Loading
    | GameOver


type alias Model =
    Result String GameState


init : () -> ( Model, Cmd Msg )
init _ =
    ( Result.Ok Ready, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( Result.Ok Starting, Api.getMaze 4 4 )

        RecieveMaze maze ->
            ( Result.Ok <| Active maze, Cmd.none )

        RecieveError error ->
            let
                message =
                    case error of
                        Http.BadUrl _ ->
                            "An error occurred due to a bad url"

                        Http.Timeout ->
                            "An error occurred due to a timeout"

                        Http.NetworkError ->
                            "A network related error occurred"

                        Http.BadStatus _ ->
                            "An error occurred due to a bad status code"

                        Http.BadPayload _ _ ->
                            "Unknown body"
            in
            ( Result.Err message, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Ok gameState ->
            viewGame gameState

        Err msg ->
            viewError msg


viewGame : GameState -> Html Msg
viewGame gameState =
    case gameState of
        Ready ->
            Html.button [ onClick Msg.Start ] [ text "Start" ]

        Starting ->
            Html.h1 [] [ text "loading" ]

        Active maze ->
            Html.pre [] [ text <| Debug.toString maze ]

        Loading ->
            Debug.todo "Loading is not implemented"

        GameOver ->
            Debug.todo "Gameover is not implemented"


viewError : String -> Html Msg
viewError msg =
    Html.h1 [] [ text msg ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
