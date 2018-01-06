import Html exposing (Html, button, div, text, input)
-- import Html.Events exposing (onClick, onInput)

main : Program Never Model Msg
main =
  Html.beginnerProgram {
    model = init,
    view = view,
    update = update
  }

type Msg = Move Int Int
  | Reset

type Player  = X | O
type Cell = Occupied Player | Empty
type alias Field = List Cell
type alias Model = {
  field: Field
}

init :  Model
init = {
    field = List.map (\_ -> Empty) (List.range 1 9),
  }

view : Model -> Html Msg
view model = div [] [ text (toString model) ]

update : Msg -> Model -> Model
update msg model = model
