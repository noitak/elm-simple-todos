module SimpleTodos where

import String exposing (isEmpty)
import Html exposing (Html, div, input, ul, li, text, span)
import Html.Events exposing (onKeyDown, onClick, on, targetValue)
import Html.Attributes exposing (placeholder, value, type', checked)
import StartApp exposing (start)

-- model
type alias TodoItem = { id : Int
                      , title : String
                      , completed : Bool
                      }

type alias Model = { todos : List TodoItem
                   , field : String
                   , next : Int
                   }

initItem : Int -> String -> Bool -> TodoItem
initItem id title completed = { id = id, title = title, completed = completed }

-- update
type Action =
  None
  | UpdateField String
  | Create String
  | Completed Int
  | Remove Int

update : Action -> Model -> Model
update action model =
  case action of
   None -> model
   UpdateField str -> { model | field <- str }
   Create title -> { model | field <- ""
                           , next <- model.next + 1
                           , todos <-
                               if String.isEmpty model.field
                               then model.todos
                               else model.todos ++ [ initItem model.next title False ]
                   }
   Completed id -> { model | todos <- List.map (\t -> if t.id == id
                                then { t | completed <- not t.completed }
                                else t)
                                model.todos }
   Remove id -> { model | todos <- List.filter (\t -> not (t.id == id)) model.todos }

-- view
view : Signal.Address Action -> Model -> Html
view address model =
  div
  []
  [ input
    [ placeholder "What needs to be done?"
    , value model.field
    , on "input" targetValue (Signal.message address << UpdateField)
    , onKeyDown address (\code -> if code == 13 then Create model.field else None)
    ]
    []
  , ul
    []
    (List.map (todoItem address) model.todos)
  ]
  
todoItem : Signal.Address Action -> TodoItem -> Html
todoItem address todo = li [] [ input [ type' "checkbox"
                                      , checked todo.completed
                                      , onClick address (Completed todo.id) ] []
                              , text todo.title
                              , text " "
                              , removeButton address todo.id
                              ]

removeButton : Signal.Address Action -> Int -> Html
removeButton address id = span
                          [ onClick address (Remove id) ]
                          [ text "[x]" ]


main = StartApp.start { model = init, update = update, view = view }


init : Model
init = { todos = [ initItem 1 "豚肉を買ってくる" True
                 , initItem 2 "たまねぎを買ってくる" True
                 , initItem 3 "にんじんを買ってくる" False
                 , initItem 4 "じゃがいもを買ってくる" False
                 , initItem 5 "カレーを作る" False
                 ]
       , field = ""
       , next = 6
       }
