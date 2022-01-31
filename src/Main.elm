module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Array exposing (Array)
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
data =
  [ [0, 1, 0, 9]
  , [0, 1, 0, 0]
  , [0, 1, 3, 0]
  , [0, 1, 0, 0]
  ]


grid: Int -> Array (Array Int)
grid n =
  let
    r =
      Array.repeat n 0
  in
  Array.repeat n r


tableRowsNew =
  Array.toList ( Array.map toHtmlRow (grid 20) )


toHtmlRow: Array Int -> Html Msg
toHtmlRow arr =
  tr [] (Array.toList (Array.indexedMap cell arr) )


cell: Int -> Int -> Html Msg
cell idx value =
  td [] [ text (String.fromInt value), div [ class "pixel-index" ] [ text (String.fromInt idx) ] ]


view : Model -> Html Msg
view model =
  div []
    [ div []
      [ table [ style "padding" "0px", style "border-collapse" "separate", style "border-spacing" "0px" ]
        tableRowsNew
      ]
    ]
