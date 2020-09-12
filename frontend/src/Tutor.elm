module Tutor exposing (AdminLevel, Gender, Tutor, TutorStatus, tutorDecoder, tutorEncoder)

import Iso8601 exposing (toTime)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Time


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


datestringEncoder : Time.Posix -> String
datestringEncoder time =
    String.join "-"
        [ Time.toYear Time.utc time |> String.fromInt
        , Time.toMonth Time.utc time |> monthToInt |> String.fromInt |> String.padLeft 2 '0'
        , Time.toDay Time.utc time |> String.fromInt |> String.padLeft 2 '0'
        ]


datestringDecoder : Decode.Decoder Time.Posix
datestringDecoder =
    Decode.string
        |> Decode.andThen
            (\val ->
                case toTime val of
                    Ok posix ->
                        Decode.succeed posix

                    Err _ ->
                        Decode.fail <| "Unable to decode date string " ++ val
            )


genderDecoder : Decode.Decoder Gender
genderDecoder =
    Decode.string
        |> Decode.andThen
            (\val ->
                case String.toLower val of
                    "male" ->
                        Decode.succeed Male

                    "female" ->
                        Decode.succeed Female

                    _ ->
                        Decode.fail "Invalid gender"
            )


tutorStatusEncoder : TutorStatus -> Int
tutorStatusEncoder status =
    case status of
        Inactive ->
            0

        Active ->
            1

        New ->
            2


tutorStatusDecoder : Decode.Decoder TutorStatus
tutorStatusDecoder =
    Decode.int
        |> Decode.andThen
            (\val ->
                case val of
                    0 ->
                        Decode.succeed Inactive

                    1 ->
                        Decode.succeed Active

                    2 ->
                        Decode.succeed New

                    _ ->
                        Decode.fail "Invalid status"
            )


tutorAdminLevelEncoder : AdminLevel -> Int
tutorAdminLevelEncoder admin =
    case admin of
        LvlAdmin ->
            0

        LvlTutor ->
            1


tutorAdminLevelDecoder : Decode.Decoder AdminLevel
tutorAdminLevelDecoder =
    Decode.int
        |> Decode.andThen
            (\val ->
                case val of
                    0 ->
                        Decode.succeed LvlAdmin

                    1 ->
                        Decode.succeed LvlTutor

                    _ ->
                        Decode.fail "Invalid admin level"
            )


type Gender
    = Male
    | Female


type TutorStatus
    = Active
    | Inactive
    | New


type AdminLevel
    = LvlAdmin
    | LvlTutor


type alias Tutor =
    { id : String
    , name : String
    , email : String
    , school : String
    , dateOfBirth : Time.Posix
    , dateOfRegistration : Time.Posix
    , gender : Gender
    , status : TutorStatus
    , admin : AdminLevel
    }


tutorDecoder : Decode.Decoder Tutor
tutorDecoder =
    Decode.succeed Tutor
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "school" Decode.string
        |> Pipeline.required "dateOfBirth" datestringDecoder
        |> Pipeline.required "dateOfRegistration" datestringDecoder
        |> Pipeline.required "gender" genderDecoder
        |> Pipeline.required "status" tutorStatusDecoder
        |> Pipeline.required "admin" tutorAdminLevelDecoder


tutorEncoder : Tutor -> Encode.Value
tutorEncoder tutor =
    Encode.object
        [ ( "id", Encode.string tutor.id )
        , ( "name", Encode.string tutor.name )
        , ( "email", Encode.string tutor.email )
        , ( "dateOfBirth"
          , Encode.string <|
                datestringEncoder tutor.dateOfBirth
          )
        ]
