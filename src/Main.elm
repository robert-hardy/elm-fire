module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (class, property, style)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import List exposing (range)
import Random exposing (Generator)
import Task
import Time


fireColorsPalette =
    Array.fromList
        [ { r = 7, g = 7, b = 7 }
        , { r = 31, g = 7, b = 7 }
        , { r = 47, g = 15, b = 7 }
        , { r = 71, g = 15, b = 7 }
        , { r = 87, g = 23, b = 7 }
        , { r = 103, g = 31, b = 7 }
        , { r = 119, g = 31, b = 7 }
        , { r = 143, g = 39, b = 7 }
        , { r = 159, g = 47, b = 7 }
        , { r = 175, g = 63, b = 7 }
        , { r = 191, g = 71, b = 7 }
        , { r = 199, g = 71, b = 7 }
        , { r = 223, g = 79, b = 7 }
        , { r = 223, g = 87, b = 7 }
        , { r = 223, g = 87, b = 7 }
        , { r = 215, g = 95, b = 7 }
        , { r = 215, g = 95, b = 7 }
        , { r = 215, g = 103, b = 15 }
        , { r = 207, g = 111, b = 15 }
        , { r = 207, g = 119, b = 15 }
        , { r = 207, g = 127, b = 15 }
        , { r = 207, g = 135, b = 23 }
        , { r = 199, g = 135, b = 23 }
        , { r = 199, g = 143, b = 23 }
        , { r = 199, g = 151, b = 31 }
        , { r = 191, g = 159, b = 31 }
        , { r = 191, g = 159, b = 31 }
        , { r = 191, g = 167, b = 39 }
        , { r = 191, g = 167, b = 39 }
        , { r = 191, g = 175, b = 47 }
        , { r = 183, g = 175, b = 47 }
        , { r = 183, g = 183, b = 47 }
        , { r = 183, g = 183, b = 55 }
        , { r = 207, g = 207, b = 111 }
        , { r = 223, g = 223, b = 159 }
        , { r = 239, g = 239, b = 199 }
        , { r = 255, g = 255, b = 255 }
        ]


gridWidth =
  100

getColor n =
    let
        maybeRGB =
            Array.get n fireColorsPalette
    in
    case maybeRGB of
        Nothing ->
            { r = 0, b = 0, g = 0 }

        Just rec ->
            rec



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    Array Int


type Msg
    = Tick Time.Posix
    | RandomListMsg (List Int)


samples : Generator (List Int)
samples =
    Random.list (gridWidth * gridWidth) (Random.int 0 3)


init : () -> ( Model, Cmd Msg )
init _ =
    ( startingArray gridWidth
    , Random.generate RandomListMsg samples
    )


lastRow : Int -> Array Int
lastRow n =
    Array.repeat n 36


startingArray : Int -> Array Int
startingArray n =
    let
        body =
            Array.repeat (n * (n - 1)) 0
    in
    Array.append body (lastRow n)


a2AA : Array Int -> Array (Array Int)
a2AA arr =
    Array.fromList (List.map Array.fromList (chunks gridWidth (Array.toList arr)))


a2LL : Array Int -> List (List Int)
a2LL arr =
    chunks gridWidth (Array.toList arr)


chunks : Int -> List Int -> List (List Int)
chunks n ls =
    case ls of
        [] ->
            []

        _ ->
            List.take n ls :: chunks n (List.drop n ls)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( model
            , Random.generate RandomListMsg samples
            )

        RandomListMsg ls ->
            ( propagate2 ls model
            , Cmd.none
            )


type alias Index =
    Int


type alias Intensity =
    Int



propagate2 : List Int -> Array Intensity -> Array Intensity
propagate2 rands arr =
    let
        indices : List Index
        indices =
            List.range 0 (Array.length arr - 1)
    in
    Array.fromList (List.map2 (\r -> \i -> consider r i arr) rands indices)


consider : Int -> Index -> Array Intensity -> Intensity
consider r i arr =
    let
        iNextRow =
            i + gridWidth

        mVal =
            Array.get iNextRow arr
    in
    case mVal of
        Just val ->
            max 0 (val - r)

        Nothing ->
            36



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 40 Tick



-- VIEW


tableRows model =
    List.indexedMap toHtmlRow (a2LL model)


toHtmlRow : Int -> List Int -> Html Msg
toHtmlRow idx_row ls =
    tr [] (List.indexedMap (\j -> colouredCell idx_row j) ls)


indexedCell : Int -> Int -> Int -> Html Msg
indexedCell idx_row idx_col value =
    let
        idxString =
            String.fromInt idx_row ++ "," ++ String.fromInt idx_col
    in
    td [] [ text (String.fromInt value), div [ class "pixel-index" ] [ text idxString ] ]


colouredCell : Int -> Int -> Int -> Html Msg
colouredCell idx_row idx_col value =
    let
        rgbData =
            getColor value

        rgbString =
            "rgb(" ++ String.fromInt rgbData.r ++ "," ++ String.fromInt rgbData.g ++ "," ++ String.fromInt rgbData.b ++ ")"
    in
    td [ class "pixel", style "background-color" rgbString ] []


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ table
                [ style "padding" "0px", style "border-collapse" "separate", style "border-spacing" "0px" ]
                (tableRows model)
            ]
        ]
