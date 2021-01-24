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
import SDate.SDate exposing (SMonth, dayFromTuple, daysInMonth, monthFromTuple, monthToTuple, weekDay)


port generate : E.Value -> Cmd msg



---- MODEL ----


type alias Model =
    { name : String
    , organization : String
    , from : String
    , to : String
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
      , organization = flags.organization
      , from = flags.from
      , to = flags.to
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
    | SetOrganization String
    | SetFrom String
    | SetTo String
    | SetYear String
    | SetDayInWeek Int Bool
    | Generate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName name ->
            ( { model | name = name }, Cmd.none )

        SetOrganization organization ->
            ( { model | organization = organization }, Cmd.none )

        SetFrom from ->
            ( { model | from = from }, Cmd.none )

        SetTo to ->
            ( { model | to = to }, Cmd.none )

        SetYear year ->
            ( { model | year = year }, Cmd.none )

        SetDayInWeek index value ->
            ( { model | days = Array.set index value model.days }, Cmd.none )

        Generate ->
            let
                value =
                    E.object
                        [ ( "name", E.string model.name )
                        , ( "organization", E.string model.organization )
                        , ( "year", E.string model.year )
                        , ( "from", E.string model.from )
                        , ( "to", E.string model.to )
                        , ( "monthsInfo", getMonthsInfo model )
                        ]
            in
            ( model, generate value )

        NoOp ->
            ( model, Cmd.none )


isHoliday : Model -> Int -> Int -> Int -> Bool
isHoliday model y m d =
    let
        matches { day, month, year } =
            case year of
                Just explicitYear ->
                    day == d && month == m && explicitYear == y

                Nothing ->
                    month == m && day == d
    in
    List.any matches model.holidays


getMonthsInfo : Model -> E.Value
getMonthsInfo model =
    let
        monthNumbers =
            List.range 1 12

        yearNumber =
            String.toInt model.year

        sMonths : Int -> List (Maybe SMonth)
        sMonths y =
            List.map (\m -> monthFromTuple ( y, m )) monthNumbers

        validMonths : List SMonth
        validMonths =
            case yearNumber of
                Just y ->
                    List.filterMap identity <| sMonths y

                Nothing ->
                    []

        monthToInfo sMonth =
            let
                ( y, m ) =
                    monthToTuple sMonth

                count =
                    daysInMonth sMonth

                firstWeekDay =
                    Maybe.withDefault 0 <| Maybe.map weekDay <| dayFromTuple ( y, m, 1 )

                nums =
                    List.range 1 count

                dayToInfo num =
                    let
                        dayInWeek =
                            remainderBy 7 (firstWeekDay + num - 1)

                        isWeekend =
                            dayInWeek == 5 || dayInWeek == 6

                        isActive =
                            Maybe.withDefault False <| Array.get dayInWeek model.days
                    in
                    E.object
                        [ ( "day", E.int num )
                        , ( "month", E.int m )
                        , ( "year", E.int y )
                        , ( "weekday", E.int dayInWeek )
                        , ( "isWeekend", E.bool isWeekend )
                        , ( "isActive", E.bool isActive )
                        , ( "isHoliday", E.bool <| isHoliday model y m num )
                        ]

                dayInfos =
                    List.map dayToInfo nums

                monthNames =
                    Array.fromList
                        [ "leden"
                        , "únor"
                        , "březen"
                        , "duben"
                        , "květen"
                        , "červen"
                        , "červenec"
                        , "srpen"
                        , "září"
                        , "říjen"
                        , "listopad"
                        , "prosinec"
                        ]
            in
            E.object
                [ ( "year", E.int y )
                , ( "month", E.int m )
                , ( "dayInfos", E.list identity dayInfos )
                , ( "monthName", E.string <| Maybe.withDefault "" <| Array.get (m - 1) monthNames )
                ]
    in
    E.list monthToInfo validMonths



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
            [ span [ class "form-label" ] [ text "Organizace" ]
            , input [ class "form-input input-organization", onInput SetOrganization, value model.organization ] []
            ]
        , label []
            [ span [ class "form-label" ] [ text "Rok" ]
            , input [ class "form-input input-year", onInput SetYear, value model.year ] []
            ]
        , div [ class "flex" ]
            [ div [ class "flex-1" ]
                [ label []
                    [ span [ class "form-label" ] [ text "Od" ]
                    , input [ class "form-input input-from", onInput SetFrom, value model.from ] []
                    ]
                ]
            , div [ class "flex-1" ]
                [ label []
                    [ span [ class "form-label" ] [ text "Do" ]
                    , input [ class "form-input input-to", onInput SetTo, value model.to ] []
                    ]
                ]
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
