module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { totalTurns : Int
    , currentTurn : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 5 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = NextTurn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextTurn ->
            ( { model | currentTurn = model.currentTurn + 1 }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "border" "6px dashed purple"
        , style "border-radius" "20px"
        , style "width" "480px"
        , style "margin" "100px auto"
        , style "padding" "40px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ span [] [ text (String.fromInt model.currentTurn) ]
        , button [ onClick NextTurn ] [ text "Next Turn" ]
        ]
