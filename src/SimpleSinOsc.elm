module SimpleSinOsc exposing (..)

import Browser
import Graph
import Html.Styled
    exposing
        ( Html
        , div
        , text
        , toUnstyled
        )
import SinOsc
import Time



-- MODEL


type alias Model =
    SinOsc.SinOscUI



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = styledView
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { osc = Just { frequency = 440.0, phase = 0.0 }
      , wFreq = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (case model.osc of
            Just osc ->
                [ buildSinOscView model
                , Graph.chart 200 <| getDrawingFunc osc
                ]

            Nothing ->
                [ text "rien à afficher" ]
        )


styledView =
    view >> toUnstyled


buildSinOscView =
    SinOsc.buildSinOscView
        "A SinOsc"
        { incrementPitch = IncrementPitch
        , decrementPitch = DecrementPitch
        , changePitch = ChangePitch
        , changePhase = ChangePhase
        }



-- UPDATE


type Msg
    = IncrementPitch
    | DecrementPitch
    | IncrementPhase
    | DecrementPhase
    | ChangePitch String
    | ChangePhase String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePitch newPitch ->
            case model.osc of
                Just osc ->
                    case String.toFloat newPitch of
                        Just newVal ->
                            ( { model
                                | osc = Just { osc | frequency = newVal }
                                , wFreq =
                                    if String.fromFloat newVal /= newPitch then
                                        Just newPitch

                                    else
                                        Nothing
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | wFreq = Just newPitch }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        IncrementPitch ->
            case model.osc of
                Just osc ->
                    ( { model
                        | osc = Just { osc | frequency = osc.frequency + 1 }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DecrementPitch ->
            case model.osc of
                Just osc ->
                    ( { model
                        | osc = Just { osc | frequency = osc.frequency - 1 }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        IncrementPhase ->
            case model.osc of
                Just osc ->
                    ( { model
                        | osc =
                            Just
                                { osc
                                    | phase =
                                        if osc.phase < 359 then
                                            osc.phase + 1

                                        else
                                            0
                                }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DecrementPhase ->
            case model.osc of
                Just osc ->
                    ( { model
                        | osc =
                            Just
                                { osc
                                    | phase =
                                        if osc.phase > 0 then
                                            osc.phase - 1

                                        else
                                            359
                                }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangePhase val ->
            case model.osc of
                Just osc ->
                    case String.toInt val of
                        Just newPhase ->
                            ( { model
                                | osc =
                                    Just
                                        { osc
                                            | phase =
                                                toFloat newPhase / 100.0
                                        }
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- GRAPH


getDrawingFunc : SinOsc.SinOsc -> (Int -> Graph.Info)
getDrawingFunc sig =
    let
        phi =
            sig.phase * pi / 180

        period =
            if sig.frequency == 0.0 then
                2.0

            else
                2.0 / sig.frequency

        samples =
            200.0
    in
    \x ->
        let
            t =
                period / samples * toFloat x
        in
        { x = t
        , y =
            sin <|
                2
                    * pi
                    * sig.frequency
                    * t
                    + phi
        }
