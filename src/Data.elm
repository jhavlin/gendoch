module Data exposing (Flags, PublicHoliday)


type alias Flags =
    { holidays : List PublicHoliday
    , year : String
    , organization : String
    , from : String
    }


type alias PublicHoliday =
    { day : Int
    , month : Int
    , year : Maybe Int
    }
