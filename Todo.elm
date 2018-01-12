module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { todos : List String
    , data : String
    }


model : Model
model =
    { todos = [ "fake_todo_01", "fake_todo_02", "..." ]

    {- right now, each todo will be considered a string,
       no other metadata will be considered like which todo number this is..
       or if the todo is completed
    -}
    , data = "..." -- this holds whatever the user will be typing into an empty task slot
    }


view =
    div []
        [ h1 [] [ text "Todo" ]
        , div [ class "todos-box" ]
            [ p [] [ text "our todos will go here" ]
            ]
        ]


main : Html msg
main =
    view
