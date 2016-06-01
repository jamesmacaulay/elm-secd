module Main exposing (..)

import SECD exposing (SECD)
import SECD.Expressions
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    { machineState : SECD
    , history : List SECD
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { machineState = SECD.constructMachine (SECD.Expressions.onePlusTwo)
      , history = []
      , error = Nothing
      }
    , Cmd.none
    )



--  UPDATE


type Msg
    = StepForward
    | StepBack


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StepForward ->
            case SECD.transform model.machineState of
                Ok newState ->
                    ( { machineState = newState
                      , history = model.machineState :: model.history
                      , error = Nothing
                      }
                    , Cmd.none
                    )

                Err errorMessage ->
                    ( { model | error = Just errorMessage }
                    , Cmd.none
                    )

        StepBack ->
            case model.history of
                [] ->
                    ( { model | error = Just "can't go back any further!" }, Cmd.none )

                previousState :: restOfHistory ->
                    ( { machineState = previousState
                      , history = restOfHistory
                      , error = Nothing
                      }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "padding", "20px" ) ] ]
        [ headerView
        , controlsView model
        , machineStateView model.machineState
        ]


headerView : Html Msg
headerView =
    h1 [] [ text "elm-secd" ]


controlsView : Model -> Html Msg
controlsView model =
    div []
        [ button [ onClick StepBack ] [ text "StepBack" ]
        , button [ onClick StepForward ] [ text "StepForward" ]
        , span [] [ text " " ]
        , errorView model.error
        ]


errorView : Maybe String -> Html Msg
errorView maybeError =
    span
        [ style [ ( "color", "red" ) ]
        ]
        [ maybeError |> Maybe.withDefault "" |> text ]


machineStateView : SECD -> Html Msg
machineStateView ( stack, env, control, dump ) =
    div []
        [ div []
            [ h4 [] [ text "stack" ]
            , ul [] (List.map (toString >> text >> (\t -> li [] [ t ])) stack)
            ]
        , div []
            [ h4 [] [ text "environment" ]
            , ul [] (List.map (toString >> text >> (\t -> li [] [ t ])) env)
            ]
        , div []
            [ h4 [] [ text "control" ]
            , ul [] (List.map (toString >> text >> (\t -> li [] [ t ])) control)
            ]
        , div []
            [ h4 [] [ text "dump" ]
            , div [] [ dump |> toString |> text ]
            ]
        ]
