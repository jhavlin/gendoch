module Decoders exposing (decodeFlags)

import Data exposing (Flags, PublicHoliday)
import Json.Decode as D
import Result


holidaysDecoder : D.Decoder (List PublicHoliday)
holidaysDecoder =
    let
        holidayDecoder =
            D.map3 PublicHoliday
                (D.field "day" D.int)
                (D.field "month" D.int)
                (D.maybe (D.field "year" D.int))
    in
    D.list holidayDecoder


decodeFlags : D.Value -> Flags
decodeFlags json =
    let
        holidaysResult =
            D.decodeValue (D.field "holidays" holidaysDecoder) json

        holidays =
            Result.withDefault [] <| holidaysResult

        yearResult =
            D.decodeValue (D.field "year" D.string) json

        year =
            Result.withDefault "2021" <| yearResult

        organization =
            Result.withDefault "" <| D.decodeValue (D.field "organization" D.string) json

        from =
            Result.withDefault "8:00" <| D.decodeValue (D.field "from" D.string) json
    in
    { holidays = holidays, year = year, from = from, organization = organization }
