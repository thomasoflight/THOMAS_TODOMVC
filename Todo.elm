-- we will not be doing ports in our example. This is a simplified version.


module Todo exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none -- WTF does this mean, I know it's anonymous?
        }



-- if we were using ports and storage they would be set up here...
-- MODEL
-- This is all the data that will be available to work with and render in `view`


type alias Model =
    { entries : List Entry
    , field : String
    , uid : Int
    , visibility : String
    }


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }



{- we would be making an emptyModel variable, instead each time we will init
   from new
-}
{- By adding a visibility field to the model we can change
   which todo items appear using CSS classes.
-}


newEntry : String -> Int -> List Entry
newEntry desc id =
    [ { description = desc
      , completed = False
      , editing = False
      , id = id
      }
    ]


<<<<<<< HEAD
=======
init : ( Model, Cmd Msg )
init =
    ( Model [] "" 0 "all", Cmd.none )



-- UPDATE


>>>>>>> 0.9.2
type Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String


update msg model =
    case msg of
        NoOp ->
            model ! []

        Add ->
            { model
                | uid = model.uid + 1
                , entries =
                    if String.isEmpty model.field then
                        model.entries
                    else
                        model.entries ++ (newEntry model.field model.uid)
                , field = ""
            }
                ! []

        UpdateField str ->
            { model | field = str } ! []

        UpdateEntry id str ->
            let
                updateDesc t =
                    if t.id == id then
                        { t | description = str }
                    else
                        t
            in
                { model | entries = (List.map updateDesc model.entries) } ! []

        Check id bool ->
            let
<<<<<<< HEAD
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
=======
                completed todo =
                    if todo.id == id then
                        { todo | completed = bool }
                    else
                        todo
            in
                { model | entries = List.map completed model.entries } ! []
>>>>>>> 0.9.2

        CheckAll bool ->
            let
                updateEntry t =
                    { t | completed = bool }
            in
                { model | entries = List.map updateEntry model.entries } ! []

        -- if List.all .completed model.entries then
        --     { model | entries = List.map (\t -> { t | completed = False }) model.entries } ! []
        -- else
        --     { model | entries = List.map (\t -> { t | completed = True }) model.entries } ! []
        Delete id ->
            { model | entries = List.filter (\t -> t.id /= id) model.entries }
                ! []

        DeleteComplete ->
            { model | entries = List.filter (\t -> t.completed /= True) model.entries } ! []

        ChangeVisibility str ->
            { model | visibility = str } ! []

        EditingEntry id bool ->
            let
                editDesc todo =
                    if todo.id == id then
                        { todo | editing = bool }
                    else
                        todo
            in
                { model | entries = List.map editDesc model.entries }
                    ! [ Task.attempt (\_ -> NoOp) (Dom.focus ("todo-" ++ toString id))
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
<<<<<<< HEAD
        [ section
            [ class "todos-box" ]
            [ lazy2 renderMainInput model.todoInputData model.todoItems
            , lazy2 renderTodoItems model.todoItems model.visibility
            , lazy renderFilters model
=======
        -- class "todomvc-wrapper" we don't have a sidebar on our cloned
        -- so it's probably ok to leave this out
        [ section [ class "todoapp" ]
            [ lazy viewInput model.field
            , lazy2 viewEntries model.visibility model.entries
            , lazy2 viewControls model.visibility model.entries
>>>>>>> 0.9.2
            ]
        , infoFooter
        ]


<<<<<<< HEAD

-- we moved allComplete to a new section
-- moving from general div to more semantic html tags e.g. section/header/footer...


renderMainInput : String -> List TodoItem -> Html Msg
renderMainInput todoInputData todoItems =
    header
        []
        [ h1 [] [ text "todos" ]
=======
viewInput : String -> Html Msg
viewInput field =
    header []
        [ h1 [] [ text "Todo" ]
>>>>>>> 0.9.2
        , input
            [ class "todo-insert-new"
            , placeholder "Pizza?"
            , autofocus True
            , value field
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
<<<<<<< HEAD
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
=======
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL THE ENTRIES


viewEntries : String -> List Entry -> Html Msg
viewEntries visibility entries =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        allCompleted =
            List.all .completed entries

        cssVisibility =
            if List.isEmpty entries then
>>>>>>> 0.9.2
                "hidden"
            else
                "visible"
    in
        section
<<<<<<< HEAD
            [ class "rendered-todos"
=======
            [ class "main"
>>>>>>> 0.9.2
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
<<<<<<< HEAD
                , checked allComplete
                , onClick ToggleAllComplete
=======
                , checked allCompleted
                , onClick (CheckAll (not allCompleted))
>>>>>>> 0.9.2
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
<<<<<<< HEAD
            , Keyed.ul [ class "todo-items" ] <|
                List.map viewKeyedEntry (List.filter forRender todoItems)
=======
            , Keyed.ul [ class "todo-list" ] <|
                List.map viewKeyedEntry (List.filter isVisible entries)
>>>>>>> 0.9.2
            ]



<<<<<<< HEAD
-- VIEW EACH TODO


viewKeyedEntry : TodoItem -> ( String, Html Msg )
viewKeyedEntry todo =
    ( toString todo.uid, lazy renderOneTodo todo )


renderOneTodo : TodoItem -> Html Msg
renderOneTodo todo =
    li
        [ classList [ ( "completed", todo.isComplete ), ( "editing", todo.isEditing ) ] ]
=======
-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
    ( toString todo.id, lazy viewEntry todo )


viewEntry : Entry -> Html Msg
viewEntry todo =
    li
        [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
>>>>>>> 0.9.2
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
<<<<<<< HEAD
                , checked todo.isComplete
                , onClick (ToggleComplete todo.uid (not todo.isComplete))
=======
                , checked todo.completed
                , onClick (Check todo.id (not todo.completed))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry todo.id True) ]
                [ text todo.description ]
            , button
                [ class "destroy"
                , onClick (Delete todo.id)
>>>>>>> 0.9.2
                ]
                []
            ]
        , input
            [ class "edit"
            , value todo.description
            , name "title"
            , id ("todo-" ++ toString todo.id)
            , onInput (UpdateEntry todo.id)
            , onBlur (EditingEntry todo.id False)
            , onEnter (EditingEntry todo.id False)
            ]
<<<<<<< HEAD
            []
=======
            [ text (todo.description) ]
>>>>>>> 0.9.2
        ]



-- VIEW CONTROLS AND FOOTER


viewControls visibility entries =
    div []
        [ button [ onClick (DeleteComplete) ] [ text <| toString <| List.length entries ]
        , button [ onClick (ChangeVisibility "all") ] [ text "all" ]
        , button [ onClick (ChangeVisibility "active") ] [ text "active" ]
        , button [ onClick (ChangeVisibility "completed") ] [ text "completed" ]
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p []
            [ text "Cloned from Todo MVC by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
