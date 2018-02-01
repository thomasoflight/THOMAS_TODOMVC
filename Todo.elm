module Todo exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    , data : String
    , uidCounter : Int
    , visibility : String
    }


type alias TodoItem =
    { desc : String
    , isComplete : Bool
    , uid : Int
    }


newTodo : String -> Int -> List TodoItem
newTodo userInput uid =
    [ { desc = userInput
      , isComplete = False
      , uid = uid
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
    | ToggleComplete Int Bool
    | MarkCompleteAll
    | DeleteOneTodo Int
    | DeleteCompleted
    | ChangeVisibility String


update msg model =
    case msg of
        NoOp ->
            model ! []

        Add ->
            { model
                | uidCounter = model.uidCounter + 1
                , todoItems =
                    if String.isEmpty model.data then
                        model.todoItems
                    else
                        model.todoItems ++ (newTodo model.data model.uidCounter)
                , data = ""
            }
                ! []

        UpdateField str ->
            { model | data = str } ! []

        ToggleComplete uid bool ->
            let
                isCompleted todo =
                    if todo.uid == uid then
                        { todo | isComplete = bool }
                    else
                        todo
            in
                { model | todoItems = List.map isCompleted model.todoItems } ! []

        MarkCompleteAll ->
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


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Todo" ]
        , div [ class "todos-box" ]
            [ viewInput model.data
            , viewTodos model.todoItems model.visibility
            , footer []
                [ button [ onClick (DeleteCompleted) ] []
                , button [ onClick (ChangeVisibility "all") ] [ text "all" ]
                , button [ onClick (ChangeVisibility "active") ] [ text "active" ]
                , button [ onClick (ChangeVisibility "completed") ] [ text "completed" ]
                , p [] [ text (model.visibility) ]
                ]
            ]
        ]


viewTodos : List TodoItem -> String -> Html Msg
viewTodos todoItems visibility =
    let
        renderEntry todo =
            div
                [ case visibility of
                    "active" ->
                        if todo.isComplete then
                            class "hidden"
                        else
                            class "active"

                    "completed" ->
                        if (not todo.isComplete) then
                            class "hidden"
                        else
                            class "active"

                    _ ->
                        class "active"
                ]
                [ li
                    []
                    [ input
                        [ type_ "checkbox"
                        , checked todo.isComplete
                        , onClick (ToggleComplete todo.uid (not todo.isComplete))
                        ]
                        []
                    , label [] [ text (todo.desc) ]
                    , button [ class "delete-one-todo", onClick (DeleteOneTodo todo.uid) ] []
                    ]
                ]
    in
        ul [ class "todo-items" ]
            (List.map
                renderEntry
                todoItems
            )


viewInput : String -> Html Msg
viewInput data =
    div []
        [ input
            [ type_ "checkbox"
            , checked False
            , onClick MarkCompleteAll
            ]
            []
        , input
            [ class "todo-insert-new"
            , type_ "text"
            , placeholder "What needs to be done?"
            , autofocus True
            , value data
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]
