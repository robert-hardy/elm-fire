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
import List exposing (range)
import Task
import Time



fireColorsPalette =
  Array.fromList
    [ {r=7, g=7, b=7}
    , {r=31, g=7, b=7}
    , {r=47, g=15, b=7}
    , {r=71, g=15, b=7}
    , {r=87, g=23, b=7}
    , {r=103, g=31, b=7}
    , {r=119, g=31, b=7}
    , {r=143, g=39, b=7}
    , {r=159, g=47, b=7}
    , {r=175, g=63, b=7}
    , {r=191, g=71, b=7}
    , {r=199, g=71, b=7}
    , {r=223, g=79, b=7}
    , {r=223, g=87, b=7}
    , {r=223, g=87, b=7}
    , {r=215, g=95, b=7}
    , {r=215, g=95, b=7}
    , {r=215, g=103, b=15}
    , {r=207, g=111, b=15}
    , {r=207, g=119, b=15}
    , {r=207, g=127, b=15}
    , {r=207, g=135, b=23}
    , {r=199, g=135, b=23}
    , {r=199, g=143, b=23}
    , {r=199, g=151, b=31}
    , {r=191, g=159, b=31}
    , {r=191, g=159, b=31}
    , {r=191, g=167, b=39}
    , {r=191, g=167, b=39}
    , {r=191, g=175, b=47}
    , {r=183, g=175, b=47}
    , {r=183, g=183, b=47}
    , {r=183, g=183, b=55}
    , {r=207, g=207, b=111}
    , {r=223, g=223, b=159}
    , {r=239, g=239, b=199}
    , {r=255, g=255, b=255}
    ]


getColor n =
  let
    maybeRGB =
      Array.get n fireColorsPalette

  in
  case maybeRGB of
    Nothing ->
      {r=0, b=0, g=0}

    Just rec ->
      rec

-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model = Array (Array Int)


init : () -> (Model, Cmd Msg)
init _ =
  ( grid 20
  , Cmd.none
  )


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


indices: Int -> List (Int, Int)
indices n =
  List.map (\r -> (r * n, r * n + (n-1))) (range 0 (n-1))


take: Int -> Array Int -> (Array Int, Array Int)
take n arr =
  let
    head =
      Array.slice 0 (n-1) arr

    tail =
      Array.slice n 1000 arr
  in
  (head, tail)


taker: (Array (Array Int), Array Int) -> (Array (Array Int), Array Int)
taker (a, b) =
  let
    (head, tail) = take 20 b
  in
  (Array.push head a, tail)


-- UPDATE


type Msg
  = Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick t ->
      ( propagate model
      , Cmd.none
      )


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



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick



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


colouredCell: Int -> Int -> Int -> Html Msg
colouredCell idx_row idx_col value =
  let
    rgbData =
      getColor value

    rgbString =
      "rgb(" ++ (String.fromInt rgbData.r) ++ "," ++ (String.fromInt rgbData.g) ++ "," ++ (String.fromInt rgbData.b) ++ ")"
  in
  td [ style "background-color" rgbString ] []


view : Model -> Html Msg
view model =
  div []
    [ div []
      [ table
        [ style "padding" "0px", style "border-collapse" "separate", style "border-spacing" "0px" ]
        ( tableRows model )
      ]
    ]
