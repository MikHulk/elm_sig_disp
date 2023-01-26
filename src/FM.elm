module FM exposing (..)

import Browser
import Css exposing (..)
import Graph
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onInput)
import SinOsc
    exposing
        ( SinOsc
        , SinOscUI
        )



-- MODEL


type alias Model =
    { carrier : SinOscUI
    , modulator : SinOscUI
    , modulationCoef : Float
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = styledView
        , subscriptions = \_ -> Sub.none
        }


initSinOscDiag : Float -> Float -> SinOscUI
initSinOscDiag freq phase =
    { osc = Just <| SinOsc freq phase
    , wFreq = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { carrier = initSinOscDiag 100.0 0.0
      , modulator = initSinOscDiag 75.0 0.0
      , modulationCoef = 0.5
      }
    , Cmd.none
    )



-- VIEW


styledView =
    view >> toUnstyled


view : Model -> Html Msg
view model =
    div []
        [ div
            [ Attrs.css
                [ displayFlex
                , justifyContent spaceBetween
                , width (px 490)
                , padding (px 7)
                ]
            ]
            [ case model.carrier.osc of
                Just osc ->
                    buildCarrierView model.carrier

                Nothing ->
                    text "rien à afficher"
            , modSlider model
            , case model.modulator.osc of
                Just osc ->
                    buildModulatorView model.modulator

                Nothing ->
                    text "rien à afficher"
            ]
        , case model.carrier.osc of
            Just carrier ->
                case model.modulator.osc of
                    Just mod ->
                        let
                            period =
                                if mod.frequency == 0.0 then
                                    3.0

                                else
                                    3.0 / mod.frequency

                            samples =
                                1000

                            graph =
                                Graph.chart samples
                        in
                        div []
                            [ graph <|
                                getDrawingFuncMult
                                    samples
                                    period
                                    model.modulationCoef
                                    carrier
                                    mod
                            , graph <| getDrawingFunc samples period carrier
                            , graph <| getDrawingFunc samples period mod
                            ]

                    Nothing ->
                        text "rien"

            Nothing ->
                text "rien"
        ]


buildCarrierView =
    SinOsc.buildSinOscView
        "Carrier SinOsc"
        { incrementPitch = IncrementCarrierPitch
        , decrementPitch = DecrementCarrierPitch
        , changePitch = ChangeCarrierPitch
        , changePhase = ChangeCarrierPhase
        }


buildModulatorView =
    SinOsc.buildSinOscView
        "Modulator SinOsc"
        { incrementPitch = IncrementModulatorPitch
        , decrementPitch = DecrementModulatorPitch
        , changePitch = ChangeModulatorPitch
        , changePhase = ChangeModulatorPhase
        }


modSlider : Model -> Html Msg
modSlider model =
    div
        [ Attrs.css
            [ backgroundColor <| rgb 127 127 255
            , width (px 40)
            ]
        ]
        [ styledLabelValue []
            [ input
                [ Attrs.type_ "range"
                , Attrs.min "0"
                , Attrs.max "10000"
                , Attrs.attribute "orient" "vertical"
                , Attrs.attribute "style" "-webkit-appearance: slider-vertical;"
                , Attrs.value <|
                    String.fromInt <|
                        Basics.round <|
                            (*) 100.0 <|
                                model.modulationCoef
                , onInput ChangeModulationCoef
                , Attrs.css
                    [ marginTop (px 19)
                    , height (px 100)
                    , width (px 35)
                    ]
                ]
                []
            ]
        , div [ Attrs.css [ margin (px 8) ] ]
            [ text (String.fromFloat model.modulationCoef) ]
        ]



-- UPDATE


type Msg
    = IncrementCarrierPitch
    | DecrementCarrierPitch
    | ChangeCarrierPitch String
    | ChangeCarrierPhase String
    | IncrementModulatorPitch
    | DecrementModulatorPitch
    | ChangeModulatorPitch String
    | ChangeModulatorPhase String
    | ChangeModulationCoef String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeModulationCoef newCoef ->
            case String.toFloat newCoef of
                Just coef ->
                    ( { model | modulationCoef = coef / 100.0 }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChangeCarrierPitch newPitch ->
            case String.toFloat newPitch of
                Just newVal ->
                    ( { model | carrier = changeOscFreq newPitch newVal model.carrier }
                    , Cmd.none
                    )

                Nothing ->
                    let
                        carrier =
                            model.carrier
                    in
                    ( { model | carrier = { carrier | wFreq = Just newPitch } }
                    , Cmd.none
                    )

        IncrementCarrierPitch ->
            ( { model | carrier = updateOscFreq ((+) 1) model.carrier }
            , Cmd.none
            )

        DecrementCarrierPitch ->
            ( { model | carrier = updateOscFreq (\f -> f - 1) model.carrier }
            , Cmd.none
            )

        ChangeCarrierPhase val ->
            case String.toFloat val of
                Just newPhase ->
                    ( { model | carrier = changeOscPhase (newPhase / 100.0) model.carrier }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangeModulatorPitch newPitch ->
            case String.toFloat newPitch of
                Just newVal ->
                    ( { model | modulator = changeOscFreq newPitch newVal model.modulator }
                    , Cmd.none
                    )

                Nothing ->
                    let
                        modulator =
                            model.modulator
                    in
                    ( { model | modulator = { modulator | wFreq = Just newPitch } }
                    , Cmd.none
                    )

        IncrementModulatorPitch ->
            ( { model | modulator = updateOscFreq ((+) 1) model.modulator }
            , Cmd.none
            )

        DecrementModulatorPitch ->
            ( { model | modulator = updateOscFreq (\f -> f - 1) model.modulator }
            , Cmd.none
            )

        ChangeModulatorPhase val ->
            case String.toFloat val of
                Just newPhase ->
                    ( { model | modulator = changeOscPhase (newPhase / 100.0) model.modulator }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


changeOscFreq : String -> Float -> SinOscUI -> SinOscUI
changeOscFreq newPitch newVal model =
    case model.osc of
        Just osc ->
            { model
                | osc = Just { osc | frequency = newVal }
                , wFreq =
                    if String.fromFloat newVal /= newPitch then
                        Just newPitch

                    else
                        Nothing
            }

        Nothing ->
            model


updateOscFreq : (Float -> Float) -> SinOscUI -> SinOscUI
updateOscFreq func model =
    case model.osc of
        Just osc ->
            { model
                | osc = Just { osc | frequency = func osc.frequency }
            }

        Nothing ->
            model


changeOscPhase : Float -> SinOscUI -> SinOscUI
changeOscPhase newVal model =
    case model.osc of
        Just osc ->
            { model
                | osc = Just { osc | phase = newVal }
            }

        Nothing ->
            model



-- GRAPH


getDrawingFuncMult : Int -> Float -> Float -> SinOsc.SinOsc -> SinOsc.SinOsc -> (Int -> Graph.Info)
getDrawingFuncMult samples period coef carrier modulator =
    let
        carrierPhi =
            carrier.phase * pi / 180

        modPhi =
            modulator.phase * pi / 180
    in
    \x ->
        let
            t =
                period / toFloat samples * toFloat x
        in
        { x = t
        , y =
            sin <|
                2
                    * pi
                    * carrier.frequency
                    * t
                    + (sin <| coef * 2 * pi * modulator.frequency * t + modPhi)
                    + carrierPhi
        }


getDrawingFunc : Int -> Float -> SinOsc.SinOsc -> (Int -> Graph.Info)
getDrawingFunc samples period sig =
    let
        phi =
            sig.phase * pi / 180
    in
    \x ->
        let
            t =
                period / toFloat samples * toFloat x
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



-- STYLES


styledLabelValue :
    List (Attribute msg)
    -> List (Html msg)
    -> Html msg
styledLabelValue =
    styled div
        [ color (rgb 45 23 23)
        , property "display" "grid"
        , property "grid-auto-row" "20px"
        , width (px 200)
        , marginTop (px 3)
        , marginBottom (px 6)
        ]
