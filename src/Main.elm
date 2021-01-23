port module Main exposing (Model, Msg(..), init, inputForm, main, update, view)

import Array exposing (Array)
import Browser
import Data exposing (PublicHoliday)
import Decoders exposing (decodeFlags)
import Html exposing (Html, button, div, h2, input, label, span, text)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Maybe


port generate : E.Value -> Cmd msg



---- MODEL ----


type alias Model =
    { name : String
    , year : String
    , days : Array Bool
    , holidays : List PublicHoliday
    , currentYear : Int
    }


init : D.Value -> ( Model, Cmd Msg )
init flagsJson =
    let
        flags =
            decodeFlags flagsJson

        days =
            Array.repeat 5 True
    in
    ( { name = ""
      , holidays = flags.holidays
      , currentYear = flags.year
      , year = String.fromInt flags.year
      , days = days
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | SetName String
    | SetYear String
    | SetDayInWeek Int Bool
    | Generate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName name ->
            ( { model | name = name }, Cmd.none )

        SetYear year ->
            ( { model | year = year }, Cmd.none )

        SetDayInWeek index value ->
            ( { model | days = Array.set index value model.days }, Cmd.none )

        Generate ->
            let
                value =
                    E.object []
            in
            ( model, generate value )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


dayNames : List String
dayNames =
    [ "pondělí", "úterý", "středa", "čtvrtek", "pátek" ]


view : Model -> Html Msg
view model =
    div []
        [ inputForm model
        , daysView model
        , generateView model
        , holidays model
        ]


inputForm : Model -> Html Msg
inputForm model =
    div []
        [ label []
            [ span [ class "form-label" ] [ text "Jméno člověka" ]
            , input [ class "form-input input-name", onInput SetName, value model.name ] []
            ]
        , label []
            [ span [ class "form-label" ] [ text "Rok" ]
            , input [ class "form-input input-year", onInput SetYear, value model.year ] []
            ]
        ]


daysView : Model -> Html Msg
daysView model =
    let
        dayWithIndexToRow index value =
            let
                isChecked : Bool
                isChecked =
                    Maybe.withDefault False <| Array.get index model.days
            in
            div []
                [ label []
                    [ input
                        [ type_ "checkbox"
                        , checked isChecked
                        , onCheck <| SetDayInWeek index
                        ]
                        []
                    , text value
                    ]
                ]

        dayRows =
            List.indexedMap dayWithIndexToRow dayNames
    in
    div [ class "days" ]
        dayRows


generateView : Model -> Html Msg
generateView _ =
    div [ class "flex" ]
        [ div [ class "flex-1" ] []
        , div [ class "flex-0" ]
            [ button [ onClick Generate ] [ text "Generuj" ]
            ]
        ]


holidays : Model -> Html Msg
holidays model =
    let
        yearAsString : Maybe Int -> String
        yearAsString maybeInt =
            Maybe.withDefault "*" <| Maybe.map String.fromInt maybeInt

        holidayRow : PublicHoliday -> Html Msg
        holidayRow holiday =
            div [ class "holiday-row" ]
                [ div
                    [ class "flex" ]
                    [ div [ class "flex-1" ]
                        [ span [] [ text <| String.fromInt holiday.day ]
                        , text ". "
                        , span [] [ text <| String.fromInt holiday.month ]
                        , text ". "
                        , span [] [ text <| yearAsString holiday.year ]
                        ]
                    , div [ class "flex-0" ]
                        [ text "-"
                        ]
                    ]
                ]
    in
    div [ class "holidays-box" ]
        [ div [ class "flex" ]
            [ div [ class "flex-1" ] [ h2 [] [ text "Holidays" ] ]
            , div [ class "flex-0" ] [ text "+" ]
            ]
        , div [ class "holidays-list" ] (List.map holidayRow model.holidays)
        ]



---- PROGRAM ----


main : Program D.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
