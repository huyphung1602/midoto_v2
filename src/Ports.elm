port module Ports exposing (storeTodos, toggleSound, ringTheBell)

port storeTodos : String -> Cmd msg
port toggleSound : Bool -> Cmd msg

port ringTheBell : Bool -> Cmd msg