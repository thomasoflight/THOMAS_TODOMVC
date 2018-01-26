module Todo exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


main : Program Never Model Msg



-- I'd like to understand what this does


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none -- WTF does this mean, I know it's anonymous?
        }


init : ( Model, Cmd msg )
init =
    ( Model [] "" 0, Cmd.none )


type alias Model =
    { todoItems : List TodoItem
    , data : String
    , uidCounter : Int
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



-- would also like to know what this A M thing is


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
    | UpdateField String
    | ToggleComplete Int Bool
    | Add


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



{- Run this function against entries that aren't checked
   In an 'ObjOr language' this would be something like entries[id], we
   might try to modify it directly. In Elm however we promised we would
   always return the same tuple of (Model, Cmd msg)
-}
-- I'd like to know more about this virtual dom thing
-- I keep hearing about it and I want to better understand virtualization
-- Msg's vs msg's. What is the deal friends? The deal is this:
-- If you imagine that your functions and their data are like a tree,
-- you must step through the code and see all the places `view` goes to.
-- A brief tour of `view`:
-- 1) all the div [] and h1 [] shit is Html msg stuff. it's the basic
--    building block for making our pages. Just think of it as a 1:1 translation of <brackets>
-- 2) viewTodos and viewInput are like big branches departing from the branch of `view`.
--    what are their annotations? As you'll see they are different.
--    viewInput returns one of our handcrafted Starbucks Msg's.
-- 3) To sum it up: you must walk the tree. In this case we know that view is always
--    going to call viewInput which we know always returns a Msg. therefore view is
--    is always going to return a Msg, otherwise it would mean that viewInput was broken
-- 3b)  to illustrate this further, you can take out viewInput and change the annotation
--      to msg and it's totally fine (it compiles) because now `view` and `viewTodos` are both returning
--      the same shit, Html msgs. Html msg = basic building block, bracket stuff. Html Msg
--      is the the same Html stuff as msg (which is anything), but it's special


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Todo" ]
        , div [ class "todos-box" ]
            [ viewInput model.data
            , viewTodos model.todoItems
            ]
        ]



-- Msg vs. msg has to do with using functions in the view. The functions viewInput


viewTodos : List TodoItem -> Html Msg
viewTodos todoItems =
    let
        renderEntry todo =
            li [ class "todo-items" ]
                [ input
                    [ type_ "checkbox"
                    , checked todo.isComplete
                    , onClick (ToggleComplete todo.uid (not todo.isComplete))
                    ]
                    []
                , label [ style [ ( "padding-left", "12px" ) ] ] [ text (todo.desc ++ " " ++ (toString todo.isComplete)) ]
                ]
    in
        div []
            [ ul
                []
                (List.map
                    renderEntry
                    todoItems
                )
            ]


viewInput : String -> Html Msg
viewInput data =
    div []
        [ input
            [ class "todo-items"
            , placeholder "What needs to be done?"
            , value data
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]



-- I don't really understand how or why main words the way it does.
-- One more thing to the database of things to learn about
-- ok
