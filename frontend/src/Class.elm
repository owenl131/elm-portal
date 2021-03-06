module Class exposing
    ( Class
    , ClassId
    , ClassSession
    , ClassTutor
    , SessionId
    , classDecoder
    , classEncoder
    , classIdDecoder
    , classIdToString
    , classSessionDecoder
    , classSessionEncoder
    , classTutorDecoder
    , emptyClass
    )

import Array exposing (empty)
import Date
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Styles exposing (dateFieldStyle)
import Time
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


classEncoder : Class -> Encode.Value
classEncoder class =
    Encode.object
        [ ( "id", Encode.string class.id )
        , ( "name", Encode.string class.name )
        , ( "year", Encode.int class.year )
        , ( "days", Encode.list (Date.weekdayToNumber >> Encode.int) class.days )
        , ( "timeslot", Encode.string class.timeslot )
        , ( "duration", Encode.float class.duration )
        , ( "active", Encode.bool class.active )
        ]


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
    , days : List Time.Weekday
    , timeslot : String
    , duration : Float
    , active : Bool
    }


emptyClass : Class
emptyClass =
    { id = ""
    , name = ""
    , year = 0
    , days = []
    , timeslot = ""
    , duration = 3
    , active = True
    }


classSessionDecoder : Decode.Decoder ClassSession
classSessionDecoder =
    Decode.succeed ClassSession
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "date" dateDecoder
        |> Pipeline.required "remarks" Decode.string
        |> Pipeline.required "duration" Decode.float


classSessionEncoder : ClassSession -> Encode.Value
classSessionEncoder session =
    Encode.object
        [ ( "id", Encode.string session.id )
        , ( "date", Encode.string (Date.toIsoString session.date) )
        , ( "remarks", Encode.string session.remarks )
        , ( "duration", Encode.float session.duration )
        ]


type alias SessionId =
    String


type alias ClassSession =
    { id : SessionId
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
