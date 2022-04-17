-- START:module
module Main exposing (main)
-- END:module

-- START:import
import Browser
import Browser.Events exposing (onKeyUp)
import Browser.Dom as Dom
import Element exposing (..)
import Element.Input as Input
import Element.Border as Border
import Html exposing (Html)
import Html.Events
import Html.Attributes as HtmlAtr
import Svg exposing (svg, rect, text_)
import Svg.Attributes as SvgAtr
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode
import Task
import Time
import Ports
import Process
-- END:import

-- START:main
main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
-- END:main

-- START:storage
saveTodos : List Todo -> Cmd msg
saveTodos todos =
    JsonEncode.list todoEncoder todos
        |> JsonEncode.encode 0
        |> Ports.storeTodos

ringTheBell : Bool -> Cmd Msg
ringTheBell isEnableBell =
    Ports.ringTheBell isEnableBell

todoEncoder : Todo -> JsonEncode.Value
todoEncoder todo =
    JsonEncode.object
        [ ( "id", JsonEncode.int todo.id )
        , ( "name", JsonEncode.string todo.name )
        , ( "workedTime", JsonEncode.float todo.workedTime )
        , ( "previousWorkedTime", JsonEncode.float todo.workedTime )
        , ( "status", JsonEncode.string <| todoStatusEncoder todo.status )
        ]

todoStatusEncoder : TodoStatus -> String
todoStatusEncoder status =
    case status of
        Active ->
            "Active"
        Incomplete ->
            "Incomplete"
        Completed ->
            "Completed"

todoStatusDecoder : JsonDecode.Decoder TodoStatus
todoStatusDecoder =
    JsonDecode.string |>
        JsonDecode.andThen
            (\str ->
                case str of
                    "Active" -> JsonDecode.succeed Active
                    "Incomplete" -> JsonDecode.succeed Incomplete
                    "Completed" -> JsonDecode.succeed Completed
                    _ -> JsonDecode.fail "Invalid TodoStatus"
            )

todoDecoder : JsonDecode.Decoder Todo
todoDecoder =
    JsonDecode.map5 Todo
        (JsonDecode.field "id" JsonDecode.int)
        (JsonDecode.field "name" JsonDecode.string)
        (JsonDecode.field "workedTime" JsonDecode.float)
        (JsonDecode.field "previousWorkedTime" JsonDecode.float)
        (JsonDecode.field "status" todoStatusDecoder)

todosDecoder : JsonDecode.Decoder (List Todo)
todosDecoder =
    JsonDecode.list todoDecoder

decodeStoredTodos : String -> List Todo
decodeStoredTodos todosJson =
    case JsonDecode.decodeString todosDecoder todosJson of
        Ok todos ->
            todos
        Err _ ->
            []

-- END:storage

-- START:model
type TodoStatus
    = Active
    | Incomplete
    | Completed

type BellStatus
    = InProgress
    | Expired

type RightPanel
    = CommandList
    | CompletedTodos

type alias Todo =
    { id : Int
    , name : String
    , workedTime : Float
    , previousWorkedTime : Float
    , status : TodoStatus
    }

type alias Model =
    { todos : List Todo
    , inputText : String
    , isShowForm : Bool
    , isWorking : Bool
    , time : Time.Posix
    , startTime : Time.Posix
    , zone : Time.Zone
    , rightPanel : RightPanel
    , enableBell : Bool
    , bellStatus : BellStatus
    }

defaultInputText : String
defaultInputText = "/"

init : Maybe String -> (Model, Cmd Msg)
init flags =
    let
        initTodos =
            case flags of
                Just todosJson ->
                    decodeStoredTodos todosJson
                Nothing ->
                    []
    in
    ( { todos = initTodos
      , inputText = defaultInputText
      , isShowForm = False
      , isWorking = False
      , time = Time.millisToPosix 0
      , startTime = Time.millisToPosix 0
      , zone = Time.utc
      , rightPanel = CommandList
      , enableBell = True
      , bellStatus = Expired
      }
    , Cmd.none
    )
-- END:model

-- START:update
type Msg
    = NoOp
    | RingBell
    | PressCharacter Char
    | PressControl String
    | ChangeInput String
    | AddTodo String
    | EditTodo Int String
    | Check Int
    | Uncheck Int
    | Delete Int
    | ActiveOn Int
    | Start Int
    | Stop
    | Show RightPanel
    | Tick Time.Posix


-- END:update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RingBell ->
            case model.bellStatus of
                InProgress ->
                    ({ model | bellStatus = Expired }, ringTheBell model.enableBell)
                _ ->
                    (model, Cmd.none)
        PressCharacter keyChar ->
            let
                cmd = if model.isShowForm then Cmd.none else Task.attempt (\_ -> NoOp) (Dom.focus "input-command-box")
            in
            ({ model | isShowForm = (keyChar == 'i' || model.isShowForm ) }
            , cmd
            )
        PressControl keyStr ->
            ({ model | isShowForm = checkControlKey keyStr model.isShowForm }
            , Cmd.none
            )
        ChangeInput input ->
            ({ model | inputText = input }
            , Cmd.none
            )
        NoOp ->
            ({ model | inputText = defaultInputText }
            , Cmd.none
            )         
        AddTodo todoName ->
            let
                newTodos = addTodos todoName model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        EditTodo index newTodoName ->
            let
                newTodos = editTodos index newTodoName model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        Check index ->
            let
                newTodos = setCompleteTodo index Completed model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        Uncheck index ->
            let
                newTodos = setCompleteTodo index Incomplete model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        Delete index ->
            let
                newTodos = deleteTodo index model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        ActiveOn index ->
            let
                newTodos = setActiveTodo index model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        Start index->
            let
                newTodos = updatePreviousWorkedTime model.todos |> startTodo index
            in
            ({ model
                | isWorking = True
                , startTime = model.time
                , todos = newTodos
                , inputText = defaultInputText
                , bellStatus = InProgress
            }
            , Cmd.batch [saveTodos newTodos, notifyIn RingBell (25*60*1000)]
            )
        Stop ->
            let
                newTodos = updatePreviousWorkedTime model.todos
            in
            ({ model
                | isWorking = False
                , inputText = defaultInputText
                , todos = newTodos
            }
            , saveTodos newTodos
            )
        Tick newTime ->
            ({ model
                | time = newTime
                , todos = if model.isWorking then updateWorkedTime model else model.todos
            }
            , Cmd.none
            )
        Show newRightPanel ->
            ({ model
                | rightPanel = newRightPanel
            }
            , Cmd.none
            )

notifyIn : Msg -> Float -> Cmd Msg
notifyIn msg time =
  Process.sleep time |> Task.attempt (\_ -> msg)

lastElem : List a -> Maybe a
lastElem list =
    case list of
        [] ->
            Nothing
        [last] ->
            Just last
        _::rest ->
            lastElem rest

addTodos : String -> List Todo -> List Todo
addTodos input todos =
    let
        lastIndex =
            case lastElem todos of
                Just t ->
                    t.id + 1
                Nothing ->
                    1
    in
    todos ++ [ Todo lastIndex input 0 0 Incomplete]

editTodos : Int -> String -> List Todo -> List Todo
editTodos index newTodoName todos =
    List.map (\todo ->
        if todo.id == index then
            { todo | name = newTodoName }
        else
            todo
    ) todos

setCompleteTodo : Int -> TodoStatus -> List Todo -> List Todo
setCompleteTodo index completedStatus todos =
    List.map (\todo ->
        if todo.id == index then
            { todo | status = completedStatus }
        else
            todo
    ) todos

setActiveTodo : Int -> List Todo -> List Todo
setActiveTodo index todos =
    List.map (\todo ->
        if todo.id == index then
            { todo | status = Active }
        else if todo.status == Active then
            { todo | status = Incomplete }
        else
            todo
    ) todos

startTodo : Int -> List Todo -> List Todo
startTodo index todos =
    List.map (\todo ->
        if todo.id == index then
            { todo |status = Active }
        else if todo.status == Active then
            { todo | status = Incomplete }
        else
            todo
    ) todos

deleteTodo : Int -> List Todo -> List Todo
deleteTodo index todos =
    List.filter (\todo -> todo.id /= index) todos

updatePreviousWorkedTime : List Todo -> List Todo
updatePreviousWorkedTime todos =
    List.map (\todo -> { todo | previousWorkedTime = todo.workedTime }) todos

updateWorkedTime : Model -> List Todo
updateWorkedTime model =
    List.map (\todo ->
        if todo.status == Active then
            { todo | workedTime = todo.previousWorkedTime + (floatFromPosix model.zone model.time) - (floatFromPosix model.zone model.startTime) }
        else
            todo
    ) model.todos

floatFromPosix : Time.Zone -> Time.Posix -> Float
floatFromPosix zone time =
    (Time.toHour zone time) * 3600 + (Time.toMinute zone time) * 60 + (Time.toSecond zone time) |> toFloat

checkControlKey : String -> Bool -> Bool
checkControlKey keyStr isShowForm =
    if isShowForm && (keyStr == "Enter" || keyStr == "Escape") then
        False
    else
        isShowForm

-- START:subscription
keyDecoder : JsonDecode.Decoder Msg
keyDecoder =
    JsonDecode.map toKey (JsonDecode.field "key" JsonDecode.string)

toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            PressCharacter char

        _ ->
            PressControl string

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ onKeySub, timerSub ]

onKeySub : Sub Msg
onKeySub =
    onKeyUp keyDecoder

timerSub : Sub Msg
timerSub =
    Time.every 1000 Tick
-- END:subscription

-- START:view
view : Model -> Html Msg
view model =
    Html.div [ HtmlAtr.style "width" "100vw", HtmlAtr.style "height" "100vh", HtmlAtr.style "overflow" "hidden" ]
        [ commandBox model
        , Html.div
            styleApplicationBody
            [ Html.div
                [ HtmlAtr.style "height" "90%"
                , HtmlAtr.style "padding" "10px 0 10px 20px"]
                [ svgLogo ]
            , Html.div styleOfLeftPanel <| (Html.h3 [HtmlAtr.style "margin-left" "20px"] [ Html.text "On Going Tasks" ])::(List.map viewTodo <| onGoingTodos model.todos)
            , Html.div styleOfRightPanel <| viewRightPanel model
            ]
        ]


-- START BLOCK:commandBox
onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (JsonDecode.field "key" JsonDecode.string
                |> JsonDecode.andThen
                    (\key ->
                        if key == "Enter" then
                            JsonDecode.succeed msg
                        else
                            JsonDecode.fail "Not the enter key"
                    )
            )
        )

displayCommandBox : Bool -> List (Html.Attribute msg)
displayCommandBox isShowForm =
    if isShowForm then
        [ HtmlAtr.style "display" "flex" ]
    else
        [ HtmlAtr.style "display" "none" ]

commandBox : Model -> Html Msg
commandBox model =
    Html.div
        ([ HtmlAtr.style "justify-content" "center"
        , HtmlAtr.style "align-items" "center"
        , HtmlAtr.style "position" "absolute"
        , HtmlAtr.style "height" "100vh"
        , HtmlAtr.style "width" "100vw"
        , HtmlAtr.style "background" "rgba(0, 0, 0, 0.5)"
        ] ++ displayCommandBox model.isShowForm)
        [ Element.layout
            []
            ( Input.text
                [ onEnter <| parseMsg (tokenize model.inputText) model.todos
                , centerX
                , centerY
                , width (px 800)
                , spacing 16
                , Border.width 2
                , Border.rounded 4
                , Border.color <| rgb255 0xd1 0xd1 0xd1
                , focused
                    [ Border.shadow
                        { offset = ( 0, 3 ), size = 10, blur = -1, color = rgb255 0xd1 0xd1 0xd1 }
                    , Border.color <| rgb255 0x0d 0xa2 0xe7
                    ]
                , Element.htmlAttribute (HtmlAtr.id "input-command-box")
                ]
                { onChange = ChangeInput
                , text = model.inputText
                , placeholder =
                    Just
                        (Input.placeholder []
                            (text "Type your task name or command")
                        )
                , label = Input.labelHidden "Command Box"
                }
            )
        ]


-- END BLOCK:commandBox


viewRightPanel : Model -> List (Html Msg)
viewRightPanel model =
    case model.rightPanel of
        CommandList ->
            viewCommandList commandElements
        CompletedTodos ->
            viewCompletedTodos model.todos

viewCompletedTodos : List Todo -> List (Html Msg)
viewCompletedTodos todos =
    (Html.h3 [HtmlAtr.style "margin-left" "20px"] [ Html.text "Completed Tasks" ])::(List.map viewTodo <| completedTodos todos)

viewCommandList : List (Html Msg) -> List (Html Msg)
viewCommandList cmdElems =
    (Html.h3 [HtmlAtr.style "margin-left" "20px"] [ Html.text "List of Commands" ])::(List.map viewCommand <| cmdElems)

viewCommand : Html Msg -> Html Msg
viewCommand cmdElement =
    Html.p [HtmlAtr.style "line-height" "32px"]
        [ Html.div
            [ HtmlAtr.style "margin-left" "16px"
            , HtmlAtr.style "margin-right" "16px"
            ]
            [ cmdElement ]
        ]

commandElements : List (Html Msg)
commandElements =
    [ Html.span []
        [ Html.strong [] [ Html.text "Press i" ]
        , Html.text " to show the command palette."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/add or /a" ]
        , Html.text " [your task name] or"
        , Html.strong [] [ Html.text " type your task name" ]
        , Html.text " to add a new task."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/edit or /e" ]
        , Html.text " [task index] [new task name] to edit a task's name."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/wk" ]
        , Html.text " [task index] to select working task."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/start" ]
        , Html.text " to start or continue counting working time on a task. "
        , Html.strong [] [ Html.text "/start" ]
        , Html.text " [task index] to select a working task and start it at the same time."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/stop" ]
        , Html.text " to stop working time on a task."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/check or /c" ]
        , Html.text " [task index] to complete a task."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/uncheck or /u" ]
        , Html.text " [task index] to complete a task."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/delete or /d" ]
        , Html.text " [task index] to delete a task."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/0" ]
        , Html.text " to show the list of commands."
        ]
    , Html.span []
        [ Html.strong [] [ Html.text "/1" ]
        , Html.text " to show the completed tasks."
        ]
    ]

viewTodo : (Int, Todo) -> Html Msg
viewTodo (uiIndex, todo) =
    Html.p []
        [ Html.div
            [ HtmlAtr.style "text-decoration"
                (if todo.status == Completed then
                    "line-through"
                else
                    "none"
                )
            , HtmlAtr.style "font-weight"
                (if todo.status == Active then
                    "600"
                else
                    "300"
                )
            , HtmlAtr.style "display" "flex"
            , HtmlAtr.style "justify-content" "space-between"
            , HtmlAtr.style "margin-left" "16px"
            , HtmlAtr.style "margin-right" "16px"
            ]
            [ Html.span [] [ Html.text <| (String.fromInt uiIndex) ++ ". " ++ todo.name ]
            , Html.span [ HtmlAtr.style "display"
                    ( if todo.status == Active || todo.status == Completed then
                        "flex"
                    else
                        "none"
                    )
                ]
                [ Html.text <| parseWorkingTimeToString todo.workedTime ]
            ]
        ]

activeOrFirstOnGoingTodos : List Todo -> List (Int, Todo)
activeOrFirstOnGoingTodos todos =
    let
        aTodos = activeTodos todos
        onTodos = onGoingTodos todos
    in
    if (List.length aTodos) > 0 then
        aTodos
    else
        onTodos

activeTodos : List Todo -> List (Int, Todo)
activeTodos todos =
    List.filter (\todo -> todo.status == Active) todos |> List.indexedMap (\x y -> (x+1, y))



onGoingTodos : List Todo -> List (Int, Todo)
onGoingTodos todos =
    List.filter (\todo -> todo.status /= Completed) todos |> List.indexedMap (\x y -> (x+1, y))

completedTodos : List Todo -> List (Int, Todo)
completedTodos todos =
    List.filter (\todo -> todo.status == Completed) todos |> List.indexedMap (\x y -> (x+1, y))

getTrueIndex : Maybe Int -> List (Int, Todo) -> Maybe Int
getTrueIndex uiIndex todoTuples =
    case todoTuples of
        [] ->
            Nothing
        [(_, x)] ->
            Just x.id
        xs ->
            List.filter (\(x, _) -> uiIndex == Just x) xs |> List.map (\(_, y) -> y.id) |> List.head

tokenize : String -> List String
tokenize input =
    String.words input

toTwoCharString : Float -> String
toTwoCharString num =
    if num > 9 then
        String.fromFloat num
    else
        "0" ++ String.fromFloat num

parseWorkingTimeToString : Float -> String
parseWorkingTimeToString workingTime =
    let
        hours = workingTime / (3600)
        hoursRounded = hours |> truncate |> toFloat
        minutes = (workingTime - hoursRounded * 3600) / 60
        minutesRouded = minutes |> truncate |> toFloat
        seconds = workingTime - hoursRounded * 3600 - minutesRouded * 60
    in
        (String.fromFloat hoursRounded) ++ ":" ++ toTwoCharString minutesRouded ++ ":" ++ toTwoCharString seconds

parseCommandUseIndex : (Int -> Msg) -> List String -> List (Int, Todo) -> Msg
parseCommandUseIndex command list todoTuples =
    let
        parseMaybeInt maybeInt =
            case maybeInt of
                Just i ->
                    command i
                Nothing ->
                    NoOp
    in
    case list of
        [] ->
            NoOp
        [x] ->
            parseMaybeInt (getTrueIndex (String.toInt x) todoTuples)
        x::_ ->
            parseMaybeInt (getTrueIndex (String.toInt x) todoTuples)

parseEditTodo : List String -> List (Int, Todo) -> Msg
parseEditTodo list todoTuples=
    case list of
        [] ->
            NoOp
        [_] ->
            NoOp
        x::xs ->
            case (getTrueIndex (String.toInt x) todoTuples) of
                Just i ->
                    EditTodo i <| String.join " " xs
                Nothing ->
                    NoOp

parseMsg : List String -> List Todo -> Msg
parseMsg list todos =
    case list of
        [] ->
            NoOp
        [x] ->
            case String.toLower x of
                "/start" ->
                    parseCommandUseIndex Start ["1"] (activeOrFirstOnGoingTodos todos)
                "/stop" ->
                    Stop
                "/0" ->
                    Show CommandList
                "/1" ->
                    Show CompletedTodos
                _ ->
                    NoOp
        x::xs ->
            case String.toLower x of
                "/add" ->
                    AddTodo <| String.join " " xs
                "/a" ->
                    AddTodo <| String.join " " xs
                "/edit" ->
                    parseEditTodo xs (onGoingTodos todos)
                "/e" ->
                    parseEditTodo xs (onGoingTodos todos)
                "/check" ->
                    parseCommandUseIndex Check xs (onGoingTodos todos)
                "/c" ->
                    parseCommandUseIndex Check xs (onGoingTodos todos)
                "/uncheck" ->
                    parseCommandUseIndex Uncheck xs (completedTodos todos)
                "/uc" ->
                    parseCommandUseIndex Uncheck xs (completedTodos todos)
                "/delete" ->
                    parseCommandUseIndex Delete xs (onGoingTodos todos)
                "/d" ->
                    parseCommandUseIndex Delete xs (onGoingTodos todos)
                "/wk" ->
                    parseCommandUseIndex ActiveOn xs (onGoingTodos todos)
                "/start" ->
                    parseCommandUseIndex Start xs (onGoingTodos todos)
                _ ->
                    AddTodo <| String.join " " list


styleApplicationBody : List (Html.Attribute msg)
styleApplicationBody =
    [ HtmlAtr.style "display" "flex"
    , HtmlAtr.style "flex-direction" "row"
    , HtmlAtr.style "align-items" "center"
    , HtmlAtr.style "width" "100%"
    , HtmlAtr.style "height" "100%"
    , HtmlAtr.style "overflow" "hidden"
    ]

styleOfLeftPanel : List (Html.Attribute msg)
styleOfLeftPanel =
    [ HtmlAtr.style "padding" "10px"
    , HtmlAtr.style "margin-left" "auto"
    , HtmlAtr.style "border" "1px solid #d1d1d1"
    , HtmlAtr.style "border-radius" "4px"
    , HtmlAtr.style "width" "40%"
    , HtmlAtr.style "height" "90%"
    , HtmlAtr.style "font-size" "16px"
    , HtmlAtr.style "overflow" "hidden"
    , HtmlAtr.style "box-shadow" "0 3px 10px rgb(0 0 0 / 0.2)"
    ]

styleOfRightPanel : List (Html.Attribute msg)
styleOfRightPanel =
    [ HtmlAtr.style "padding" "10px"
    , HtmlAtr.style "margin-left" "auto"
    , HtmlAtr.style "margin-right" "auto"
    , HtmlAtr.style "border" "1px solid #d1d1d1"
    , HtmlAtr.style "border-radius" "4px"
    , HtmlAtr.style "width" "40%"
    , HtmlAtr.style "height" "90%"
    , HtmlAtr.style "font-size" "16px"
    , HtmlAtr.style "overflow" "hidden"
    , HtmlAtr.style "box-shadow" "0 3px 10px rgb(0 0 0 / 0.2)"
    ]

-- END:view

-- START:logo
svgLogo : Html msg
svgLogo =
    svg
        [ SvgAtr.viewBox "10 10 40 40"
        , SvgAtr.width "50"
        , SvgAtr.height "50"
        , HtmlAtr.style "margin-top" "-10px"
        ]
        [ rect
            [ SvgAtr.x "10"
            , SvgAtr.y "10"
            , SvgAtr.width "40"
            , SvgAtr.height "40"
            ]
            []
        , text_
            [ SvgAtr.x "50%"
            , SvgAtr.y "50%"
            , SvgAtr.dominantBaseline "hanging"
            , SvgAtr.fill "white"
            , SvgAtr.fontFamily "Courgette, cursive"
            , SvgAtr.fontWeight "600"
            , SvgAtr.fontSize "14px"
            ]
            [
                Svg.text "Mi"
            ]
        ]
-- END:logo
