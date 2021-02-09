port module Main exposing (Model, Msg(..), init, inputForm, main, update, view)

import Array exposing (Array)
import Browser
import Components.Common exposing (confirmDeleteButton)
import Data exposing (PublicHoliday)
import Decoders exposing (decodeFlags)
import Encoders exposing (encodeHolidays)
import Html exposing (Html, button, div, h2, input, label, span, text)
import Html.Attributes exposing (checked, class, disabled, maxlength, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import List
import Maybe
import SDate.SDate exposing (SMonth, dayFromTuple, daysInMonth, monthFromTuple, monthToTuple, weekDay)


port generate : E.Value -> Cmd msg


port saveHolidays : E.Value -> Cmd msg



---- MODEL ----


type alias Model =
    { name : String
    , organization : String
    , from : String
    , jobTime : String
    , addLunch : Bool
    , year : String
    , days : Array Bool -- days in week enabled by checkboxes, index 0 = Monday
    , holidays : List PublicHoliday
    , deletedHoliday : Maybe PublicHoliday
    , addedHoliday : Maybe { day : String, month : String, year : String }
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
      , jobTime = "1,0"
      , addLunch = True
      , holidays = flags.holidays
      , year = flags.year
      , days = days
      , deletedHoliday = Nothing
      , addedHoliday = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | SetName String
    | SetOrganization String
    | SetFrom String
    | SetJobTime String
    | SetYear String
    | SetDayInWeek Int Bool
    | Generate
    | RemoveHoliday Bool PublicHoliday
    | AddHolidayStart
    | AddHolidayEdit String String
    | AddHolidayFinish
    | SetAddLunch Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName name ->
            ( { model | name = name }, Cmd.none )

        SetOrganization organization ->
            ( { model | organization = organization }, Cmd.none )

        SetFrom from ->
            ( { model | from = from }, Cmd.none )

        SetJobTime jobTime ->
            ( { model | jobTime = jobTime }, Cmd.none )

        SetAddLunch addLunch ->
            ( { model | addLunch = addLunch }, Cmd.none )

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
                        , ( "to", E.string <| computeTo model )
                        , ( "monthsInfo", getMonthsInfo model )
                        ]
            in
            ( model, generate value )

        RemoveHoliday confirmed holiday ->
            if confirmed then
                let
                    notDeletedOne h =
                        h /= holiday

                    newHolidays =
                        List.filter notDeletedOne model.holidays
                in
                ( { model | holidays = newHolidays, deletedHoliday = Nothing }, saveHolidays <| encodeHolidays newHolidays )

            else
                ( { model | deletedHoliday = Just holiday }, Cmd.none )

        AddHolidayStart ->
            ( { model | addedHoliday = Just { day = "", month = "", year = "" } }, Cmd.none )

        AddHolidayEdit field value ->
            let
                updated =
                    case model.addedHoliday of
                        Just { day, month, year } ->
                            case field of
                                "day" ->
                                    Just { day = value, month = month, year = year }

                                "month" ->
                                    Just { day = day, month = value, year = year }

                                "year" ->
                                    Just { day = day, month = month, year = value }

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
            in
            ( { model | addedHoliday = updated }, Cmd.none )

        AddHolidayFinish ->
            case model.addedHoliday of
                Just { day, month, year } ->
                    let
                        dayInt =
                            String.toInt day

                        monthInt =
                            String.toInt month

                        yearInt =
                            String.toInt year
                    in
                    case ( dayInt, monthInt ) of
                        ( Just d, Just m ) ->
                            let
                                newHolidays =
                                    model.holidays ++ [ PublicHoliday d m yearInt ]

                                newModel =
                                    { model | holidays = newHolidays, addedHoliday = Nothing }
                            in
                            ( newModel, saveHolidays <| encodeHolidays newHolidays )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


jobTimeToMinutes : Model -> Int
jobTimeToMinutes model =
    let
        coefficient =
            model.jobTime |> String.replace "," "." |> String.toFloat |> Maybe.withDefault 0.0

        base =
            toFloat <| 8 * 60

        minutes =
            round <| coefficient * base
    in
    minutes


timeToMinues : String -> Int
timeToMinues time =
    let
        parts =
            String.split ":" time

        toMinutes h m =
            if h >= 0 && h < 24 && m >= 0 && m < 60 then
                h * 60 + m

            else
                0

        asMinutes h m =
            Maybe.map2 toMinutes
                (String.toInt h)
                (String.toInt m)
    in
    case parts of
        [ h, m ] ->
            asMinutes h m |> Maybe.withDefault 0

        _ ->
            0


minutesToTime : Int -> String
minutesToTime minutes =
    let
        h =
            minutes // 60

        m =
            remainderBy 60 minutes

        pad str =
            if String.length str < 2 then
                "0" ++ str

            else
                str
    in
    String.concat [ String.fromInt h |> pad, ":", String.fromInt m |> pad ]


computeTo : Model -> String
computeTo model =
    let
        start =
            timeToMinues model.from

        duration =
            jobTimeToMinutes model

        lunchTime =
            if model.addLunch || isLunchForced model then
                30

            else
                0

        sum =
            start + duration + lunchTime

        validSum =
            remainderBy (24 * 60) sum
    in
    if start > 0 && duration > 0 then
        minutesToTime validSum

    else
        "??:??"


isLunchForced : Model -> Bool
isLunchForced _ =
    False



-- 6 hours


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
        , div [ class "flex time-controls" ]
            [ div [ class "flex-1" ]
                [ label []
                    [ span [ class "form-label" ] [ text "Od" ]
                    , input [ class "form-input input-from", onInput SetFrom, value model.from ] []
                    ]
                ]
            , div [ class "flex-1" ]
                [ label []
                    [ span [ class "form-label" ] [ text "Úvazek" ]
                    , input [ class "form-input input-to", onInput SetJobTime, value model.jobTime ] []
                    ]
                ]
            , div [ class "flex-1" ]
                [ label []
                    [ span [ class "form-label" ] [ text "Oběd" ]
                    , div [ class "middle align-items-start" ]
                        [ input
                            [ type_ "checkbox"
                            , disabled <| isLunchForced model
                            , checked <| isLunchForced model || model.addLunch
                            , onCheck <| SetAddLunch
                            ]
                            []
                        ]
                    ]
                ]
            , div [ class "flex-1" ]
                [ label []
                    [ span [ class "form-label" ] [ text "Do" ]
                    , div [ class "middle align-items-start" ] [ text <| computeTo model ]
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
                    , confirmDeleteButton
                        { isBeingDeleted = model.deletedHoliday == Just holiday
                        , startText = "×"
                        , questionText = "Smazat?"
                        , confirmText = "Ano"
                        , cancelText = "Ne"
                        , startMsg = RemoveHoliday False holiday
                        , confirmMsg = RemoveHoliday True holiday
                        , cancelMsg = RemoveHoliday True <| PublicHoliday -1 -1 Nothing
                        , attrs = [ class "flex-0" ]
                        }
                    ]
                ]

        addRow =
            div [ class "holiday-row" ]
                [ div [ class "flex" ] addRowContent ]

        addRowContent =
            case model.addedHoliday of
                Nothing ->
                    [ div [ class "flex-1" ] [ text "Přidat svátek" ]
                    , button [ class "flex-0", onClick AddHolidayStart ] [ text "+" ]
                    ]

                Just { day, month, year } ->
                    [ div [ class "flex-1" ]
                        [ input [ class "inline-input", value day, placeholder "d", maxlength 2, onInput <| AddHolidayEdit "day" ] []
                        , text ". "
                        , input [ class "inline-input", value month, placeholder "m", maxlength 2, onInput <| AddHolidayEdit "month" ] []
                        , text ". "
                        , input [ class "inline-input inline-input-long", value year, placeholder "r / *", maxlength 4, onInput <| AddHolidayEdit "year" ] []
                        ]
                    , button [ class "flex-0", onClick AddHolidayFinish ] [ text "+" ]
                    ]

        warning =
            let
                isInSpecifiedYear : PublicHoliday -> Bool
                isInSpecifiedYear ph =
                    case ph.year of
                        Just _ ->
                            ph.year == String.toInt model.year

                        _ ->
                            False

                specificSet =
                    2 == (List.length <| List.filter isInSpecifiedYear model.holidays)
            in
            if specificSet then
                div [] []

            else
                div [ class "warning-box" ]
                    [ text "Nejsou uvedeny dva svátky pro zadaný rok, možná jste zapomněli přidat letošní Velikonoce!"
                    ]
    in
    div [ class "holidays-box" ]
        [ div [ class "flex" ]
            [ div [ class "flex-1" ] [ h2 [] [ text "Státní svátky" ] ]
            ]
        , warning
        , div [ class "holidays-list" ]
            (List.map holidayRow model.holidays ++ [ addRow ])
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
