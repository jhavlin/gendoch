module Data exposing (Flags, PublicHoliday)


type alias Flags =
    { holidays : List PublicHoliday
    , year : Int
    }


type alias PublicHoliday =
    { day : Int
    , month : Int
    , year : Maybe Int
    }
