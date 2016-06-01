module Main exposing (..)

import SECD exposing (SECD)
import Html exposing (Html)
import Html.App


main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { machineState : SECD }


init : ( Model, Cmd Msg )
init =
    ( { machineState = ( [], [], [], SECD.NoDump ) }, Cmd.none )



--  UPDATE


type Msg
    = Never


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    toString model.machineState |> Html.text
