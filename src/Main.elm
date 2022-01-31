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


type alias Model = Array (Array Int)


init : Model
init =
  grid 20


lastRow n =
  Array.repeat n 36


grid: Int -> Array (Array Int)
grid n =
  let
    r =
      Array.repeat n 0

    body =
      Array.repeat (n - 1) r
  in
  Array.push ( lastRow n ) body


-- UPDATE


type Msg
  = Update


update : Msg -> Model -> Model
update msg model =
  case msg of
    Update ->
      propagate model


propagate model =
  let
    rows =
      Array.slice 1 20 model

    updated =
      Array.map decayFrom rows
  in
  Array.push ( lastRow 20 ) updated


decayFrom: Array Int -> Array Int
decayFrom row =
  Array.map (\x -> max 0 (x - 1)) row



-- VIEW
tableRows model =
  Array.toList ( Array.indexedMap toHtmlRow model )


toHtmlRow: Int -> Array Int -> Html Msg
toHtmlRow idx_row arr =
  tr [] (Array.toList (Array.indexedMap (\j -> indexedCell idx_row j ) arr) )


indexedCell: Int -> Int -> Int -> Html Msg
indexedCell idx_row idx_col value =
  let
    idxString =
      (String.fromInt idx_row) ++ "," ++ (String.fromInt idx_col)
  in
  td [] [ text (String.fromInt value), div [ class "pixel-index" ] [ text idxString ] ]


view : Model -> Html Msg
view model =
  div []
    [ div []
      [ table
        [ style "padding" "0px", style "border-collapse" "separate", style "border-spacing" "0px" ]
        ( tableRows model )
      , button [ onClick Update ] [ text "+" ]
      ]
    ]
