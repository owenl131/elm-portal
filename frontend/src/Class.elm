module Class exposing
    ( Class
    , ClassId
    , ClassSession
    , ClassTutor
    , classDecoder
    , classIdDecoder
    , classIdToString
    , classSessionDecoder
    , classTutorDecoder
    )

import Date
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Tutor


dateDecoder : Decode.Decoder Date.Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\val ->
                case Date.fromIsoString val of
                    Ok date ->
                        Decode.succeed date

                    Err error ->
                        Decode.fail error
            )


weekdayDecoder : Decode.Decoder Date.Weekday
weekdayDecoder =
    Decode.int |> Decode.map Date.numberToWeekday


classDecoder : Decode.Decoder Class
classDecoder =
    Decode.succeed Class
        |> Pipeline.required "id" classIdDecoder
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "year" Decode.int
        |> Pipeline.required "days" (Decode.list weekdayDecoder)
        |> Pipeline.required "timeslot" Decode.string
        |> Pipeline.required "duration" Decode.float
        |> Pipeline.required "active" Decode.bool


type alias ClassId =
    String


classIdDecoder : Decode.Decoder ClassId
classIdDecoder =
    Decode.string


classIdToString : ClassId -> String
classIdToString classId =
    classId


type alias Class =
    { id : ClassId
    , name : String
    , year : Int
    , days : List Date.Weekday
    , timeslot : String
    , duration : Float
    , active : Bool
    }


classSessionDecoder : Decode.Decoder ClassSession
classSessionDecoder =
    Decode.succeed ClassSession
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "date" dateDecoder
        |> Pipeline.required "remarks" Decode.string
        |> Pipeline.required "duration" Decode.float


type alias ClassSession =
    { id : Int
    , date : Date.Date
    , remarks : String
    , duration : Float
    }


classTutorDecoder : Decode.Decoder ClassTutor
classTutorDecoder =
    Decode.succeed ClassTutor
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "admin" Tutor.tutorAdminLevelDecoder
        |> Pipeline.required "joinDate" dateDecoder
        |> Pipeline.optional "leaveDate" (Decode.maybe dateDecoder) Nothing


type alias ClassTutor =
    { id : String
    , name : String
    , admin : Tutor.AdminLevel
    , joinDate : Date.Date
    , leaveDate : Maybe Date.Date
    }
