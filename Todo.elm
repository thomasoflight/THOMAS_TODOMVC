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
                isComplete todo =
                    if todo.uid == uid then
                        { todo | isComplete = bool }
                    else
                        todo
            in
                { model | todoItems = List.map isComplete model.todoItems } ! []

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



{- we don't need a "todomvc-wrapper" since we don't have a sidebar in this
   example
-}
{- we moved h1 element and placed into a subsection below.
   Since the main view function is the highest level, we want to keep
   more detailed stuff in separate functions below. The main view
   is just putting it all together for us.
-}


view : Model -> Html Msg
view model =
    div []
        [ section
            [ class "todos-box" ]
            [ lazy2 renderMainInput model.todoInputData model.todoItems
            , lazy2 renderTodoItems model.todoItems model.visibility
            , lazy renderFilters model
            ]
        , infoFooter
        ]



-- we moved allComplete to a new section
-- moving from general div to more semantic html tags e.g. section/header/footer...


renderMainInput : String -> List TodoItem -> Html Msg
renderMainInput todoInputData todoItems =
    header
        []
        [ h1 [] [ text "todos" ]
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



-- VIEW ALL TODO ITEMS


renderTodoItems : List TodoItem -> String -> Html Msg
renderTodoItems todoItems visibility =
    let
        forRender todo =
            case visibility of
                "completed" ->
                    todo.isComplete

                "active" ->
                    not todo.isComplete

                _ ->
                    True

        allComplete =
            List.all .isComplete todoItems && (not (List.isEmpty todoItems))

        cssVisibility =
            if List.isEmpty todoItems then
                "hidden"
            else
                "visible"
    in
        section
            [ class "rendered-todos"
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked allComplete
                , onClick ToggleAllComplete
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , Keyed.ul [ class "todo-items" ] <|
                List.map viewKeyedEntry (List.filter forRender todoItems)
            ]



-- VIEW EACH TODO


viewKeyedEntry : TodoItem -> ( String, Html Msg )
viewKeyedEntry todo =
    ( toString todo.uid, lazy renderOneTodo todo )


renderOneTodo : TodoItem -> Html Msg
renderOneTodo todo =
    li
        [ classList [ ( "completed", todo.isComplete ), ( "editing", todo.isEditing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
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
            []
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
