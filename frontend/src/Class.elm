module Class exposing (Class, ClassSession, ClassTutor, classDecoder)

import Date
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


weekdayDecoder : Decode.Decoder Date.Weekday
weekdayDecoder =
    Decode.int |> Decode.map Date.numberToWeekday


classDecoder : Decode.Decoder Class
classDecoder =
    Decode.succeed Class
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "days" (Decode.list weekdayDecoder)
        |> Pipeline.required "timeslot" Decode.string
        |> Pipeline.required "duration" Decode.float
        |> Pipeline.required "active" Decode.bool


type alias Class =
    { id : Int
    , name : String
    , days : List Date.Weekday
    , timeslot : String
    , duration : Float
    , active : Bool
    }



-- type alias ClassExtended =
--     { class : Class
--     , sessions : List ClassSession
--     , tutors : List ClassTutor
--     }


type alias ClassSession =
    { id : Int
    , date : Date.Date
    , remarks : String
    , duration : Float
    , present : List String
    }


type alias ClassTutor =
    { tutorId : String
    , joinDate : Date.Date
    , leaveDate : Maybe Date.Date
    }
