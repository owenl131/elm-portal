module Student exposing (..)

import Date
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Utils exposing (Gender(..))


type alias StudentId =
    String


type StudentStatus
    = Active
    | Inactive


studentStatusAsString : StudentStatus -> String
studentStatusAsString status =
    case status of
        Active ->
            "Active"

        Inactive ->
            "Inactive"


studentStatusEncoder : StudentStatus -> Int
studentStatusEncoder status =
    case status of
        Active ->
            0

        Inactive ->
            1


toStudentStatus : Int -> Maybe StudentStatus
toStudentStatus status =
    case status of
        0 ->
            Just Active

        1 ->
            Just Inactive

        _ ->
            Nothing


studentStatusDecoder : Decode.Decoder StudentStatus
studentStatusDecoder =
    Decode.int
        |> Decode.map toStudentStatus
        |> Decode.map (Maybe.map Decode.succeed)
        |> Decode.andThen (Maybe.withDefault (Decode.fail "Invalid student status"))


type SchoolType
    = SchoolTypePrimary
    | SchoolTypeSecondary
    | SchoolTypePolytechnic
    | SchoolTypeITE


schoolTypeAsString : SchoolType -> String
schoolTypeAsString school =
    case school of
        SchoolTypePrimary ->
            "Primary School"

        SchoolTypeSecondary ->
            "Secondary School"

        SchoolTypePolytechnic ->
            "Polytechnic"

        SchoolTypeITE ->
            "ITE"


schoolTypeEncoder : SchoolType -> String
schoolTypeEncoder school =
    case school of
        SchoolTypePrimary ->
            "pri"

        SchoolTypeSecondary ->
            "sec"

        SchoolTypePolytechnic ->
            "poly"

        SchoolTypeITE ->
            "ite"


toSchoolType : String -> Maybe SchoolType
toSchoolType school =
    case String.toLower school of
        "pri" ->
            Just SchoolTypePrimary

        "sec" ->
            Just SchoolTypeSecondary

        "poly" ->
            Just SchoolTypePolytechnic

        "ite" ->
            Just SchoolTypeITE

        _ ->
            Nothing


schoolTypeDecoder : Decode.Decoder SchoolType
schoolTypeDecoder =
    Decode.string
        |> Decode.map toSchoolType
        |> Decode.map (Maybe.map Decode.succeed)
        |> Decode.andThen (Maybe.withDefault (Decode.fail "Invalid student school type"))


type alias SchoolRecord =
    { year : Int
    , school : String
    , schoolType : SchoolType
    , yearOfGraduation : Int
    }


schoolRecordEncoder : SchoolRecord -> Encode.Value
schoolRecordEncoder schoolRecord =
    Encode.object
        [ ( "year", Encode.int schoolRecord.year )
        , ( "school", Encode.string schoolRecord.school )
        , ( "schoolType", Encode.string (schoolTypeEncoder schoolRecord.schoolType) )
        , ( "yearOfGraduation", Encode.int schoolRecord.yearOfGraduation )
        ]


schoolRecordDecoder : Decode.Decoder SchoolRecord
schoolRecordDecoder =
    Decode.succeed SchoolRecord
        |> Pipeline.required "year" Decode.int
        |> Pipeline.required "school" Decode.string
        |> Pipeline.required "schoolType" schoolTypeDecoder
        |> Pipeline.required "yearOfGraduation" Decode.int


type alias Student =
    { id : StudentId
    , name : String
    , dateOfBirth : Date.Date
    , dateOfRegistration : Date.Date
    , gender : Gender
    , status : StudentStatus
    , schools : List SchoolRecord
    }


emptyStudent : Student
emptyStudent =
    { id = ""
    , name = ""
    , dateOfBirth = Date.fromRataDie 1
    , dateOfRegistration = Date.fromRataDie 1
    , gender = Utils.Male
    , schools = []
    , status = Inactive
    }


studentEncoder : Student -> Encode.Value
studentEncoder student =
    Encode.object
        [ ( "id", Encode.string student.id )
        , ( "name", Encode.string student.name )
        , ( "dateOfBirth", Encode.string (Date.toIsoString student.dateOfBirth) )
        , ( "dateOfRegistration", Encode.string (Date.toIsoString student.dateOfRegistration) )
        , ( "gender", Encode.string (Utils.genderEncoder student.gender) )
        , ( "status", Encode.int (studentStatusEncoder student.status) )
        , ( "schools", Encode.list schoolRecordEncoder student.schools )
        ]


studentDecoder : Decode.Decoder Student
studentDecoder =
    Decode.succeed Student
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "dateOfBirth" Utils.dateDecoder
        |> Pipeline.required "dateOfRegistration" Utils.dateDecoder
        |> Pipeline.required "gender" Utils.genderDecoder
        |> Pipeline.required "status" studentStatusDecoder
        |> Pipeline.required "schools" (Decode.list schoolRecordDecoder)
