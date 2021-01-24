module Data exposing (Flags, PublicHoliday)


type alias Flags =
    { holidays : List PublicHoliday
    , year : Int
    , organization : String
    , from : String
    , to : String
    }


type alias PublicHoliday =
    { day : Int
    , month : Int
    , year : Maybe Int
    }
