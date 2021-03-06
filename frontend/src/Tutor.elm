module Tutor exposing
    ( AdminLevel(..)
    , Tutor
    , TutorExtended
    , TutorId
    , TutorStatus(..)
    , adminLevelAsString
    , allLanguages
    , allSubjects
    , emptyTutor
    , schoolTypeAsString
    , subjectAsString
    , toTutorAdminLevel
    , toTutorStatus
    , tutorAdminLevelDecoder
    , tutorAdminLevelEncoder
    , tutorDecoder
    , tutorEncoder
    , tutorExtendedDecoder
    , tutorExtendedEncoder
    , tutorLanguageAsString
    , tutorStatusAsString
    , tutorStatusEncoder
    )

import Date
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Student exposing (schoolTypeAsString)
import Time
import Utils


tutorStatusEncoder : TutorStatus -> Int
tutorStatusEncoder status =
    case status of
        Inactive ->
            0

        Active ->
            1

        New ->
            2


toTutorStatus : Int -> Maybe TutorStatus
toTutorStatus status =
    case status of
        0 ->
            Just Inactive

        1 ->
            Just Active

        2 ->
            Just New

        _ ->
            Nothing


tutorStatusDecoder : Decode.Decoder TutorStatus
tutorStatusDecoder =
    Decode.int
        |> Decode.map toTutorStatus
        |> Decode.map (Maybe.map Decode.succeed)
        |> Decode.andThen (Maybe.withDefault (Decode.fail "Invalid status"))


tutorStatusAsString : TutorStatus -> String
tutorStatusAsString status =
    case status of
        Active ->
            "Active"

        Inactive ->
            "Inactive"

        New ->
            "New"


tutorAdminLevelEncoder : AdminLevel -> Int
tutorAdminLevelEncoder admin =
    case admin of
        LvlAdmin ->
            0

        LvlTutor ->
            1


adminLevelAsString : AdminLevel -> String
adminLevelAsString admin =
    case admin of
        LvlAdmin ->
            "Admin"

        LvlTutor ->
            "Tutor"


toTutorAdminLevel : Int -> Maybe AdminLevel
toTutorAdminLevel lvl =
    case lvl of
        0 ->
            Just LvlAdmin

        1 ->
            Just LvlTutor

        _ ->
            Nothing


tutorAdminLevelDecoder : Decode.Decoder AdminLevel
tutorAdminLevelDecoder =
    Decode.int
        |> Decode.map toTutorAdminLevel
        |> Decode.map (Maybe.map Decode.succeed)
        |> Decode.andThen (Maybe.withDefault (Decode.fail "Invalid admin level"))


type TutorStatus
    = Active
    | Inactive
    | New


type AdminLevel
    = LvlAdmin
    | LvlTutor


type alias TutorId =
    String


type alias Tutor =
    { id : TutorId
    , name : String
    , email : String
    , school : String
    , dateOfBirth : Date.Date
    , dateOfRegistration : Date.Date
    , gender : Utils.Gender
    , status : TutorStatus
    , admin : AdminLevel
    , password : Maybe String
    }


emptyTutor : Tutor
emptyTutor =
    { id = ""
    , name = ""
    , email = ""
    , school = ""
    , dateOfBirth = Date.fromRataDie 1
    , dateOfRegistration = Date.fromRataDie 1
    , gender = Utils.Female
    , status = New
    , admin = LvlTutor
    , password = Nothing
    }


tutorDecoder : Decode.Decoder Tutor
tutorDecoder =
    Decode.succeed Tutor
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "school" Decode.string
        |> Pipeline.required "dob" Utils.dateDecoder
        |> Pipeline.required "doc" Utils.dateDecoder
        |> Pipeline.required "gender" Utils.genderDecoder
        |> Pipeline.required "status" tutorStatusDecoder
        |> Pipeline.required "admin" tutorAdminLevelDecoder
        |> Pipeline.optional "password" (Decode.map Just Decode.string) Nothing


tutorEncoder : Tutor -> Encode.Value
tutorEncoder tutor =
    Encode.object
        ([ ( "id", Encode.string tutor.id )
         , ( "name", Encode.string tutor.name )
         , ( "email", Encode.string tutor.email )
         , ( "dob", Encode.string (Date.toIsoString tutor.dateOfBirth) )
         , ( "doc", Encode.string (Date.toIsoString tutor.dateOfRegistration) )
         , ( "school", Encode.string tutor.school )
         , ( "admin", Encode.int (tutorAdminLevelEncoder tutor.admin) )
         , ( "status", Encode.int (tutorStatusEncoder tutor.status) )
         , ( "gender", Encode.string (Utils.genderEncoder tutor.gender) )
         ]
            ++ (Maybe.map (\m -> [ ( "password", Encode.string m ) ]) tutor.password
                    |> Maybe.withDefault []
               )
        )


type TutorLanguage
    = LangEnglish
    | LangChinese
    | LangMalay
    | LangTamil


allLanguages : List TutorLanguage
allLanguages =
    [ LangEnglish, LangChinese, LangMalay, LangTamil ]


tutorLanguageAsString : TutorLanguage -> String
tutorLanguageAsString lang =
    case lang of
        LangEnglish ->
            "English"

        LangChinese ->
            "Chinese"

        LangMalay ->
            "Malay"

        LangTamil ->
            "Tamil"


tutorLanguageEncoder : TutorLanguage -> Int
tutorLanguageEncoder lang =
    case lang of
        LangEnglish ->
            0

        LangChinese ->
            1

        LangMalay ->
            2

        LangTamil ->
            3


toTutorLanguage : Int -> Maybe TutorLanguage
toTutorLanguage code =
    case code of
        0 ->
            Just LangEnglish

        1 ->
            Just LangChinese

        2 ->
            Just LangMalay

        3 ->
            Just LangTamil

        _ ->
            Nothing


tutorLanguageDecoder : Decode.Decoder TutorLanguage
tutorLanguageDecoder =
    Decode.int
        |> Decode.map toTutorLanguage
        |> Decode.map (Maybe.map Decode.succeed)
        |> Decode.andThen
            (Maybe.withDefault (Decode.fail "Invalid tutor language"))


type Subject
    = English
    | Reading
    | Mathematics
    | Computing
    | Science


allSubjects : List Subject
allSubjects =
    [ English, Reading, Mathematics, Computing, Science ]


subjectAsString : Subject -> String
subjectAsString subject =
    case subject of
        English ->
            "English"

        Reading ->
            "Reading"

        Mathematics ->
            "Mathematics"

        Computing ->
            "Computing"

        Science ->
            "Science"


subjectEncoder : Subject -> Int
subjectEncoder subject =
    case subject of
        English ->
            0

        Reading ->
            1

        Mathematics ->
            2

        Computing ->
            3

        Science ->
            4


toSubject : Int -> Maybe Subject
toSubject subject =
    case subject of
        0 ->
            Just English

        1 ->
            Just Reading

        2 ->
            Just Mathematics

        3 ->
            Just Computing

        4 ->
            Just Science

        _ ->
            Nothing


subjectDecoder : Decode.Decoder Subject
subjectDecoder =
    Decode.int
        |> Decode.map toSubject
        |> Decode.map (Maybe.map Decode.succeed)
        |> Decode.andThen
            (Maybe.withDefault (Decode.fail "Invalid subject"))


type SchoolType
    = SchoolType_Primary
    | SchoolType_NA
    | SchoolType_NT
    | SchoolType_OLvl
    | SchoolType_ALvl
    | SchoolType_IB_IP
    | SchoolType_Polytechnic
    | SchoolType_ITE
    | SchoolType_University
    | SchoolType_None


schoolTypeEncoder : SchoolType -> String
schoolTypeEncoder school =
    case school of
        SchoolType_Primary ->
            "pri"

        SchoolType_NA ->
            "n_acad"

        SchoolType_NT ->
            "n_tech"

        SchoolType_OLvl ->
            "olvl"

        SchoolType_ALvl ->
            "alvl"

        SchoolType_IB_IP ->
            "ibip"

        SchoolType_Polytechnic ->
            "poly"

        SchoolType_ITE ->
            "ite"

        SchoolType_University ->
            "uni"

        SchoolType_None ->
            "none"


schoolTypeAsString : SchoolType -> String
schoolTypeAsString =
    schoolTypeEncoder


toSchoolType : String -> Maybe SchoolType
toSchoolType school =
    case school of
        "pri" ->
            Just SchoolType_Primary

        "n_acad" ->
            Just SchoolType_NA

        "n_tech" ->
            Just SchoolType_NT

        "olvl" ->
            Just SchoolType_OLvl

        "alvl" ->
            Just SchoolType_ALvl

        "ibip" ->
            Just SchoolType_IB_IP

        "poly" ->
            Just SchoolType_Polytechnic

        "ite" ->
            Just SchoolType_ITE

        "uni" ->
            Just SchoolType_University

        "none" ->
            Just SchoolType_None

        _ ->
            Nothing


schoolTypeDecoder : Decode.Decoder SchoolType
schoolTypeDecoder =
    Decode.string
        |> Decode.map toSchoolType
        |> Decode.map (Maybe.map Decode.succeed)
        |> Decode.andThen (Maybe.withDefault (Decode.fail "Invalid tutor school type"))


type alias TutorExtended =
    { languages : List TutorLanguage
    , available : List Time.Weekday
    , subjects : List Subject
    , careerGoal : String
    , schoolType : SchoolType
    , yearOfGraduation : Int
    , remarks : List String
    }


tutorExtendedEncoder : TutorExtended -> Encode.Value
tutorExtendedEncoder tutor =
    Encode.object
        [ ( "languages", Encode.list Encode.int (List.map tutorLanguageEncoder tutor.languages) )
        , ( "available", Encode.list Encode.int (List.map Date.weekdayToNumber tutor.available) )
        , ( "subjects", Encode.list Encode.int (List.map subjectEncoder tutor.subjects) )
        , ( "careerGoal", Encode.string tutor.careerGoal )
        , ( "schoolType", Encode.string (schoolTypeEncoder tutor.schoolType) )
        , ( "yearOfGraduation", Encode.int tutor.yearOfGraduation )
        , ( "remarks", Encode.list Encode.string tutor.remarks )
        ]


tutorExtendedDecoder : Decode.Decoder TutorExtended
tutorExtendedDecoder =
    Decode.succeed TutorExtended
        |> Pipeline.optional "languages" (Decode.list tutorLanguageDecoder) []
        |> Pipeline.optional "available" (Decode.list (Decode.int |> Decode.map Date.numberToWeekday)) []
        |> Pipeline.optional "subjects" (Decode.list subjectDecoder) []
        |> Pipeline.optional "careerGoal" Decode.string ""
        |> Pipeline.optional "schoolType" schoolTypeDecoder SchoolType_None
        |> Pipeline.optional "yearOfGraduation" Decode.int 0
        |> Pipeline.optional "remarks" (Decode.list Decode.string) []
