module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, table, tbody, td, tr, text)
import Html.Attributes exposing (class, style, property)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import List exposing (range)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Int


init : Model
init =
  0



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW
twoCols =
  List.map cell (range 1 2)


cell idx =
  td [] [ text "b", div [ class "pixel-index" ] [ text (String.fromInt idx) ] ]


view : Model -> Html Msg
view model =
  div []
    [ div []
      [ table [ style "padding" "0px", style "border-collapse" "separate", style "border-spacing" "0px" ]
        [ tr [] twoCols
        , tr [] twoCols
        , tr [] twoCols
        ]
      ]
    ]
