module Api exposing (getMaze)

import Http
import HttpBuilder exposing (..)
import Maze
import Msg exposing (..)


handleRequestComplete : (a -> Msg) -> Result Http.Error a -> Msg
handleRequestComplete toMsg result =
    case result of
        Ok data ->
            toMsg data

        Err error ->
            RecieveError error


getMaze : Int -> Int -> Cmd Msg
getMaze row col =
    HttpBuilder.get ("/mazes/" ++ String.fromInt row ++ "/" ++ String.fromInt col)
        |> withTimeout 10000
        |> withExpectJson Maze.decoder
        |> send (handleRequestComplete RecieveMaze)
