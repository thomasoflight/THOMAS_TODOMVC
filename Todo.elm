module Todo exposing (..)

import Dom
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none -- WTF does this mean, I know it's anonymous?
        }


init : ( Model, Cmd msg )
init =
    ( Model [] "" 0 "all", Cmd.none )



{- By adding a visibility field to the model we can change
   which todo items appear using CSS classes.
-}


type alias Model =
    { todoItems : List TodoItem
    , todoInputData : String
    , uidCounter : Int
    , visibility : String
    }


type alias TodoItem =
    { desc : String
    , isComplete : Bool
    , uid : Int
    , isEditing : Bool
    }


newTodo : String -> Int -> List TodoItem
newTodo userInput uid =
    [ { desc = userInput
      , isComplete = False
      , uid = uid
      , isEditing = False
      }
    ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)



-- andThen? JSON? decoders?
-- are there other Msg's beside those listed below?


type Msg
    = NoOp
    | Add
    | UpdateField String
    | UpdateDesc Int String
    | ToggleComplete Int Bool
    | ToggleAllComplete
    | DeleteOneTodo Int
    | DeleteCompleted
    | ChangeVisibility String
    | EditEntry Int Bool


update msg model =
    case msg of
        NoOp ->
            model ! []

        Add ->
            { model
                | uidCounter = model.uidCounter + 1
                , todoItems =
                    if String.isEmpty model.todoInputData then
                        model.todoItems
                    else
                        model.todoItems ++ (newTodo model.todoInputData model.uidCounter)
                , todoInputData = ""
            }
                ! []

        UpdateField str ->
            { model | todoInputData = str } ! []

        UpdateDesc uid str ->
            let
                updateDesc t =
                    if t.uid == uid then
                        { t | desc = str }
                    else
                        t
            in
                { model | todoItems = (List.map updateDesc model.todoItems) } ! []

        ToggleComplete uid bool ->
            let
                isCompleted todo =
                    if todo.uid == uid then
                        { todo | isComplete = bool }
                    else
                        todo
            in
                { model | todoItems = List.map isCompleted model.todoItems } ! []

        ToggleAllComplete ->
            if List.all .isComplete model.todoItems then
                { model | todoItems = List.map (\t -> { t | isComplete = False }) model.todoItems } ! []
            else
                { model | todoItems = List.map (\t -> { t | isComplete = True }) model.todoItems } ! []

        DeleteOneTodo uid ->
            { model | todoItems = List.filter (\t -> t.uid /= uid) model.todoItems }
                ! []

        DeleteCompleted ->
            { model | todoItems = List.filter (\t -> t.isComplete /= True) model.todoItems } ! []

        ChangeVisibility str ->
            { model | visibility = str } ! []

        EditEntry uid bool ->
            let
                editDesc todo =
                    if todo.uid == uid then
                        { todo | isEditing = bool }
                    else
                        todo
            in
                { model | todoItems = List.map editDesc model.todoItems }
                    ! [ Task.attempt (\_ -> NoOp) (Dom.focus ("todo-" ++ toString uid))
                      ]


view : Model -> Html Msg
view model =
    div []
        -- class "todomvc-wrapper" we don't have a sidebar on our cloned
        -- so it's probably ok to leave this out
        [ section []
            [ div
                [ class "todos-box"
                ]
                {- we moved h1 element and placed into a subsection below.
                   Since the main view function is the highest level, we want to keep
                   more detailed stuff in separate functions below. The main view
                   is just putting it all together for us.
                -}
                [ lazy2 renderMainInput model.todoInputData model.todoItems
                , lazy2 renderTodoItems model.todoItems model.visibility
                , lazy renderFilters model
                ]
            , infoFooter
            ]
        ]


renderMainInput : String -> List TodoItem -> Html Msg
renderMainInput todoInputData todoItems =
    -- we moved allComplete to a new section
    -- moving from general div to more semantic html tags e.g. section/header/footer...
    header []
        [ h1 [] [ text "Todo" ]
        , input
            [ class "todo-insert-new"
            , placeholder "What needs to be done?"
            , autofocus True
            , value todoInputData
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


renderTodoItems : List TodoItem -> String -> Html Msg
renderTodoItems todoItems visibility =
    let
        forRender =
            case visibility of
                "active" ->
                    List.filter (\a -> a.isComplete /= True) todoItems

                "completed" ->
                    List.filter .isComplete todoItems

                _ ->
                    todoItems

        allComplete =
            List.all .isComplete todoItems && (not (List.isEmpty todoItems))
    in
        section []
            [ input
                [ type_ "checkbox"
                , checked allComplete
                , onClick ToggleAllComplete
                ]
                []
            , label [ for "toggle-all" ] [ text "Mark all as complete" ]
            , ul [ class "todo-items" ] <| List.map renderOneTodo forRender
            ]


renderOneTodo : TodoItem -> Html Msg
renderOneTodo todo =
    li
        [ classList [ ( "editing", todo.isEditing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ type_ "checkbox"
                , checked todo.isComplete
                , onClick (ToggleComplete todo.uid (not todo.isComplete))
                ]
                []
            , label [ onDoubleClick (EditEntry todo.uid True) ] [ text todo.desc ]
            , button [ class "delete-one-todo", onClick (DeleteOneTodo todo.uid) ] [ text "x" ]
            ]
        , input
            [ class "edit"
            , value todo.desc
            , name "title"
            , id ("todo-" ++ toString todo.uid)
            , onInput (UpdateDesc todo.uid)
            , onBlur (EditEntry todo.uid False)
            , onEnter (EditEntry todo.uid False)
            ]
            [ text (todo.desc) ]
        ]


renderFilters model =
    div []
        [ button [ onClick (DeleteCompleted) ] [ text <| toString <| List.length model.todoItems ]
        , button [ onClick (ChangeVisibility "all") ] [ text "all" ]
        , button [ onClick (ChangeVisibility "active") ] [ text "active" ]
        , button [ onClick (ChangeVisibility "completed") ] [ text "completed" ]
        ]


infoFooter : Html msg
infoFooter =
    footer []
        [ p [] [ text "Some basic footer info" ]
        ]
