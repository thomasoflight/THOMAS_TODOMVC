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
            , viewEntries
            ]
        ]


viewEntries =
    let
        renderEntry entry =
            li [] [ text (toString entry) ]
    in
        div [] (List.map renderEntry model.todos)



-- THE DEAL WITH 'let / in' --
{- Ok so anytime you see 'let' it means, "everything until the 'in' is a building block I'm going to use in the part
    after the 'in' " Just like 'lambda' functions, 'let' gives you a temporary namespace to define variables,
   functions etc. Compare the alternative below, which does the same thing:
    `
            renderEntry entry =
                li [] [ text (toString entry) ]

            viewEntries =
                div [] (List.map renderEntry model.todos)

    BUT it's kind of weird right? I mean one line per function? And they go together kind right?
    Doesn't that maybe feel weird like it would be nice to combine them?
    If you feel this way, Let / In is your friend. viewEntries in this case is like,
    "hey I need a function for this other function.
    Lemme make that first and then I can use it for the List.map which needs a function..."
-}


main : Html msg
main =
    view
