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


init : ( Model, Cmd Msg )
init =
    ( Model [] "" 0 "all", Cmd.none )



-- UPDATE


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
                completed todo =
                    if todo.id == id then
                        { todo | completed = bool }
                    else
                        todo
            in
                { model | entries = List.map completed model.entries } ! []

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


view : Model -> Html Msg
view model =
    div []
        -- class "todomvc-wrapper" we don't have a sidebar on our cloned
        -- so it's probably ok to leave this out
        [ section [ class "todoapp" ]
            [ lazy viewInput model.field
            , lazy2 viewEntries model.visibility model.entries
            , lazy2 viewControls model.visibility model.entries
            ]
        , infoFooter
        ]


viewInput : String -> Html Msg
viewInput field =
    header []
        [ h1 [] [ text "Todo" ]
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
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked allCompleted
                , onClick (CheckAll (not allCompleted))
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , Keyed.ul [ class "todo-list" ] <|
                List.map viewKeyedEntry (List.filter isVisible entries)
            ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
    ( toString todo.id, lazy viewEntry todo )


viewEntry : Entry -> Html Msg
viewEntry todo =
    li
        [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
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
            [ text (todo.description) ]
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
