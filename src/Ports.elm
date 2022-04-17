port module Ports exposing (storeTodos, ringTheBell)

port storeTodos : String -> Cmd msg

port ringTheBell : Bool -> Cmd msg