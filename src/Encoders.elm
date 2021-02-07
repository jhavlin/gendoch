module Encoders exposing (encodeHolidays)

import Data exposing (PublicHoliday)
import Json.Encode as E


encodeHolidays : List PublicHoliday -> E.Value
encodeHolidays holidays =
    let
        encodeHoliday ph =
            case ph.year of
                Just y ->
                    E.object
                        [ ( "day", E.int ph.day )
                        , ( "month", E.int ph.month )
                        , ( "year", E.int y )
                        ]

                Nothing ->
                    E.object
                        [ ( "day", E.int ph.day )
                        , ( "month", E.int ph.month )
                        ]
    in
    E.list encodeHoliday holidays
