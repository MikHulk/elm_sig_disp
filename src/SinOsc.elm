module SinOsc exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onClick, onInput)



-- TYPES


type alias SinOsc =
    { frequency : Float
    , phase : Float
    }


type alias SinOscUI =
    { osc : Maybe SinOsc
    , wFreq : Maybe String
    }



-- BUILDER


buildSinOscView : String -> EventMapping msg -> SinOscUI -> Html msg
buildSinOscView name mapping model =
    div
        [ Attrs.css
            [ borderRadius (px 5)
            , backgroundColor (hex "#f2d2f2")
            , padding (px 10)
            , width (px 200)
            ]
        ]
        [ h3 [] [ text name ]
        , styledLabelValue []
            [ styledLabel [] [ text "frequency" ]
            , button
                [ Attrs.css
                    [ height (px 16)
                    , paddingBottom (px 19)
                    , marginTop (px 7)
                    , borderStyle solid
                    , borderWidth (px 1)
                    , Css.hover [ backgroundColor (rgb 125 200 254) ]
                    ]
                , onClick mapping.decrementPitch
                ]
                [ text "-" ]
            , button
                [ Attrs.css
                    [ height (px 16)
                    , paddingBottom (px 19)
                    , marginTop (px 7)
                    , borderStyle solid
                    , borderWidth (px 1)
                    , Css.hover [ backgroundColor (rgb 125 200 254) ]
                    ]
                , onClick mapping.incrementPitch
                ]
                [ text "+" ]
            , styledVar
                [ Attrs.value <| extractFreq model
                , onInput mapping.changePitch
                , Attrs.css
                    [ case model.wFreq of
                        Just _ ->
                            color (rgb 255 0 0)

                        Nothing ->
                            color (rgb 0 0 0)
                    ]
                ]
                []
            ]
        , styledLabelValue []
            [ styledLabel [] [ text "phase" ]
            , styledValue []
                [ text <|
                    String.fromFloat <|
                        extractPhase model
                ]
            ]
        , styledLabelValue []
            [ input
                [ Attrs.type_ "range"
                , Attrs.min "0"
                , Attrs.max "36000"
                , Attrs.value <|
                    String.fromInt <|
                        Basics.round <|
                            (*) 100.0 <|
                                extractPhase model
                , onInput mapping.changePhase
                , Attrs.css
                    [ property "grid-row" "2"
                    , property "grid-column-start" "1"
                    , property "grid-column-end" "2"
                    ]
                ]
                []
            ]
        ]



-- EXTRACT


extractFreq : SinOscUI -> String
extractFreq model =
    case model.wFreq of
        Just val ->
            val

        Nothing ->
            case model.osc of
                Just osc ->
                    String.fromFloat osc.frequency

                Nothing ->
                    ""


extractPhase : SinOscUI -> Float
extractPhase model =
    case model.osc of
        Just osc ->
            osc.phase

        Nothing ->
            0.0



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


styledLabel :
    List (Attribute msg)
    -> List (Html msg)
    -> Html msg
styledLabel =
    styled div
        [ fontWeight bold
        , width (px 100)
        , property "grid-row" "1"
        , property "grid-column" "1"
        ]


styledVar :
    List (Attribute msg)
    -> List (Html msg)
    -> Html msg
styledVar =
    styled input
        [ property "grid-row" "1"
        , property "grid-column" "2"
        , width (px 90)
        , textAlign right
        , paddingTop (px 2)
        , paddingBottom (px 2)
        , borderRadius (px 6)
        , borderStyle hidden
        , padding2 (px 3) (px 4)
        , fontSize (px 13)
        , marginTop (px -2)
        ]


styledValue :
    List (Attribute msg)
    -> List (Html msg)
    -> Html msg
styledValue =
    styled div
        [ property "grid-row" "1"
        , property "grid-column" "2"
        , width (px 95)
        , textAlign right
        ]


type alias EventMapping msg =
    { decrementPitch : msg
    , incrementPitch : msg
    , changePitch : String -> msg
    , changePhase : String -> msg
    }
