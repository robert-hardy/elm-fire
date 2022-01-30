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


view : Model -> Html Msg
view model =
  div []
    [ div []
      [ table [ style "padding" "0px", style "border-collapse" "separate", style "border-spacing" "0px" ]
        [ tr []
          [ td [] [ text "a", div [ class "pixel-index" ] [ text "1" ] ]
          , td [] [ text "b", div [ class "pixel-index" ] [ text "1" ] ]
          ]
        , tr []
          [ td [] [ text "c", div [ class "pixel-index" ] [ text "1" ] ]
          , td [] [ text "d", div [ class "pixel-index" ] [ text "1" ] ]
          ]
        ]
      ]
    ]
