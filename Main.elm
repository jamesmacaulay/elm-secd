module Main exposing (..)

import SECD exposing (SECD)
import SECD.Examples
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
    { machine : SECD.Machine
    , history : List SECD.Machine
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { machine = SECD.Examples.plusAndMinus
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
            case SECD.transform model.machine of
                Ok machine ->
                    ( { machine = machine
                      , history = model.machine :: model.history
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

                previousMachine :: restOfHistory ->
                    ( { machine = previousMachine
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
        , machineStateView model.machine.state
        ]


headerView : Html Msg
headerView =
    h1 [] [ text "elm-secd" ]


controlsView : Model -> Html Msg
controlsView model =
    div []
        [ button [ onClick StepBack, disabled (List.isEmpty model.history) ] [ text "StepBack" ]
        , button [ onClick StepForward, disabled (SECD.isDone model.machine) ] [ text "StepForward" ]
        , span [] [ text " " ]
        , doneView model.machine
        , errorView model.error
        ]


doneView : SECD.Machine -> Html Msg
doneView machine =
    span [ style [ ( "color", "green" ) ] ]
        [ if SECD.isDone machine then
            text "done!"
          else
            text ""
        ]


errorView : Maybe String -> Html Msg
errorView maybeError =
    span [ style [ ( "color", "red" ) ] ]
        [ maybeError |> Maybe.withDefault "" |> text ]


machineStateView : SECD.SECD -> Html Msg
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
