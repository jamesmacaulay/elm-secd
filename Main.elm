module Main exposing (..)

import SECD exposing (SECD)
import SECD.Examples
import SECD.Printer
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App
import Json.Decode as Json
import Dict exposing (Dict)


main =
    Html.App.program
        { init = initDefault
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { machine : SECD.Machine
    , initMachine : SECD.Machine
    , history : List SECD.Machine
    , error : Maybe String
    , selectedMachineName : String
    }


exampleMachines : List ( String, SECD.Machine )
exampleMachines =
    [ ( "HelloWorld", SECD.Examples.helloWorld )
    , ( "(λx.x)(HelloWorld)", SECD.Examples.helloWorldIdentity )
    , ( "(λa.λb.a)(True)(False)", SECD.Examples.showChurchTrue )
    , ( "(λa.λb.b)(True)(False)", SECD.Examples.showChurchFalse )
    , ( "Church encoding: 0 == 0", SECD.Examples.zeroIsZero )
    , ( "Church encoding: (1 - 1) == 0", SECD.Examples.oneMinusOneIsZero )
    , ( "Church encoding: (2 - 1) != 0", SECD.Examples.twoMinusOneIsNotZero )
    , ( "1 + 2", SECD.Examples.onePlusTwo )
    , ( "(11 - 2) + 5", SECD.Examples.plusAndMinus )
    ]


exampleMachinesDict : Dict String SECD.Machine
exampleMachinesDict =
    Dict.fromList exampleMachines


init : ( String, SECD.Machine ) -> ( Model, Cmd Msg )
init ( machineName, machine ) =
    ( { machine = machine
      , initMachine = machine
      , history = []
      , error = Nothing
      , selectedMachineName = machineName
      }
    , Cmd.none
    )


defaultNamedMachine : ( String, SECD.Machine )
defaultNamedMachine =
    ( "HelloWorld", SECD.Examples.helloWorld )


initDefault : ( Model, Cmd Msg )
initDefault =
    init defaultNamedMachine



--  UPDATE


type Msg
    = StepForward
    | StepBack
    | ResetMachine String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StepForward ->
            case SECD.transform model.machine of
                Ok machine ->
                    ( { model
                        | machine = machine
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
                    ( { model
                        | machine = previousMachine
                        , history = restOfHistory
                        , error = Nothing
                      }
                    , Cmd.none
                    )

        ResetMachine machineName ->
            case Dict.get machineName exampleMachinesDict of
                Nothing ->
                    initDefault

                Just machine ->
                    init ( machineName, machine )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "padding", "20px" ) ] ]
        [ headerView
        , machineSelectorView model
        , machineInfoView model
        , controlsView model
        , machineStateView model.machine.state
        ]


headerView : Html Msg
headerView =
    div []
        [ h1 [] [ text "elm-secd" ]
        , p [] [ a [ href "https://github.com/jamesmacaulay/elm-secd" ] [ text "source on github" ] ]
        , p []
            [ text "This is an "
            , a [ href "http://elm-lang.org/" ]
                [ text "Elm" ]
            , text " implementation of Peter Landin's "
            , a [ href "https://en.wikipedia.org/wiki/SECD_machine" ]
                [ text "SECD machine" ]
            , text ", as described his 1964 paper "
            , a [ href "https://www.cs.cmu.edu/afs/cs/user/crary/www/819-f09/Landin64.pdf" ]
                [ text "The Mechanical Evaluation of Expressions" ]
            , text "."
            ]
        ]


machineSelectorView : Model -> Html Msg
machineSelectorView model =
    div []
        [ text "Load example machine: "
        , select [ on "change" (Json.map ResetMachine targetValue) ]
            (exampleMachines
                |> List.map
                    (\( name, _ ) ->
                        option [ selected (name == model.selectedMachineName) ] [ text name ]
                    )
            )
        ]


machineInfoView : Model -> Html Msg
machineInfoView model =
    div []
        [ div []
            (case model.initMachine.state of
                ( _, _, (SECD.AE expression) :: _, _ ) ->
                    [ text ("Initial expression: " ++ SECD.Printer.expressionToString expression) ]

                _ ->
                    []
            )
        , div []
            (if Dict.isEmpty model.initMachine.basicFunctions then
                []
             else
                [ text ("Built-in basic functions available: " ++ toString (Dict.keys model.initMachine.basicFunctions)) ]
            )
        ]


controlsView : Model -> Html Msg
controlsView model =
    div [ style [ ( "margin-top", "20px" ) ] ]
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
    let
        toText =
            toString >> text

        toListItems =
            List.map (\x -> li [] [ toText x ])
    in
        div []
            [ div []
                [ h4 [] [ text "stack" ]
                , ul [] (toListItems stack)
                ]
            , div []
                [ h4 [] [ text "environment" ]
                , ul [] (toListItems env)
                ]
            , div []
                [ h4 [] [ text "control" ]
                , ul [] (toListItems control)
                ]
            , div []
                [ h4 [] [ text "dump" ]
                , div [] [ toText dump ]
                ]
            ]
