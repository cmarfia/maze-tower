module Msg exposing (Msg(..))

import Http
import Maze exposing (Maze)


type Msg
    = Start
    | RecieveMaze Maze
    | RecieveError Http.Error
