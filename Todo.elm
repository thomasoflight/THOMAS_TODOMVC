module Todo exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { todoItems : List TodoItem
    , data : String
    }


type alias TodoItem =
    { desc : String -- 1)
    , isComplete : Bool -- 2)
    , uniqueId : Int -- 3)
    }


newTodo userInput uid =
    [ { desc = userInput
      , isComplete = False
      , uniqueId = uid
      }
    ]


type Msg
    = NoOp
    | Add
    | UpdateField String


model : Model
model =
    { todoItems = []
    , data = "(like a blank line)"
    }


update msg model =
    case msg of
        NoOp ->
            model

        Add ->
            let
                todo =
                    newTodo model.data (List.length model.todoItems)
            in
                { model
                    | todoItems =
                        if String.isEmpty model.data then
                            model.todoItems
                        else
                            model.todoItems ++ todo
                }

        UpdateField str ->
            { model | data = str }


view model =
    div []
        [ h1 [] [ text "Todo" ]
        , div [ class "todos-box" ]
            [ viewInput model
            , viewTodos model
            ]
        ]


viewTodos model =
    let
        renderEntry ({ desc, isComplete, uniqueId } as todoItem) =
            li [] [ text (String.join " " [ (toString desc), (toString uniqueId), (toString isComplete) ]) ]
    in
        div [ style [ ( "list-style", "none" ) ] ]
            [ p
                [ style
                    [ ( "font-weight", "bold" )
                    ]
                ]
                [ text "Todo Name | Todo Id | Todo Status" ]
            , ul [] (List.map renderEntry model.todoItems)
            ]


viewInput model =
    div []
        [ input
            [ type_ "text"
            , placeholder "What needs to be done?"
            , name "newTodo"
            , onInput UpdateField
            ]
            []
        , button [ onClick Add ] [ text "Add Todo" ]
        ]



-- I don't really understand how or why main words the way it does.
-- One more thing to the database of things to learn about
-- ok


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
