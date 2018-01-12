module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { todoItems : List TodoItem -- now we have a list of aliased records!
    , data : String
    }



{- what is an alias?
   An alias is shorthand for a larger piece of code
   Each item on a todo list will have more than piece of data associated with it. Unlike a hand written list
   where we cross off todo items and can see their order visually we need a way represent that in computer code.
   Because we don't use computers by literally looking at bits code could be considered NOT visual medium.
   So in the alias below 'TodoItem' we are using a record to store all the data that would typically
   we would just know by looking at a real todo list. Things you do to todo lists, generally:
       1) write down something to do [a description]
       2) cross off an item or mark it as complete in some other way like a check mark [an indicator of completeness]
       3) move items around by erasing them and rewriting elsewhere
          --or--
          sometimes numbering todo items and just erasing and rewriting numbers (computers do this way faster than people)
           [computers use something called 'uid's or "unique identifiers"]

           * a 'uid' in that sense like an alias in it's own right! It's a number that means another thing.
            ** a todo item here will mean a record with a particular format
   Let's make us a record, yo!!
-}


type alias TodoItem =
    { desc : String -- 1)
    , isComplete : Bool -- 2)
    , uniqueId : Int -- 3)
    }


todoZero : TodoItem
todoZero =
    { desc = "my todoooooo! "
    , isComplete = False
    , uniqueId = 0
    }



-- usually people start at one, but since this is for a computer, we'll start with this funny name, todoZero


todoOne : TodoItem
todoOne =
    { desc = "my todo twooo "
    , isComplete = False
    , uniqueId = 1
    }


model : Model
model =
    { todoItems = [ todoZero, todoOne ]
    , data = "(like a blank line)"
    }



{- right now, each todo will be considered a string,
    no other metadata will be considered like which todo number this is..
    or if the todo is completed

   'data' holds whatever the user will be typing into an empty task slot, consider it like
    the next blank line on a physical todo list

    'uniqueId' we haven't made any todo items yet, so the first todo will be number 0. Computers
    are strange and start at 0. Zero's history is cray: [ https://yaleglobal.yale.edu/history-zero ]
-}


view =
    div []
        [ h1 [] [ text "Todo" ]
        , div [ class "todos-box" ]
            [ p [] [ text "our todos will go here" ]
            , viewTodos
            ]
        ]


viewTodos =
    let
        renderEntry ({ desc, isComplete, uniqueId } as todoItem) =
            li [] [ text (String.join " " [ (toString desc), (toString uniqueId), (toString isComplete) ]) ]
    in
        div [] (List.map renderEntry model.todoItems)



-- THE DEAL WITH 'let / in' --
{- Ok so anytime you see 'let' it means, "everything until the 'in' is a building block I'm going to use in the part
    after the 'in' " Just like 'lambda' functions, 'let' gives you a temporary namespace to define variables,
   functions etc. Compare the alternative below, which does the same thing:
    `
            renderEntry entry =
                li [] [ text (toString entry) ]

            viewTodos =
                div [] (List.map renderEntry model.todos)

    BUT it's kind of weird right? I mean one line per function? And they go together kind right?
    Doesn't that maybe feel weird like it would be nice to combine them?
    If you feel this way, Let / In is your friend. viewTodos in this case is like,
    "hey I need a function for this other function.
    Lemme make that first and then I can use it for the List.map which needs a function..."
-}


main : Html msg
main =
    view
