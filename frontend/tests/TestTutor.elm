module TestTutor exposing (..)

import Date
import Expect
import Fuzz exposing (Fuzzer, int, list, string)
import Http exposing (Expect)
import Json.Decode
import Json.Encode
import Test exposing (..)
import Time
import Tutor
import Utils


suite : Test
suite =
    let
        tutor : Tutor.Tutor
        tutor =
            { id = "abcd1234"
            , admin = Tutor.LvlTutor
            , name = "Test Name"
            , dateOfBirth = Date.fromWeekDate 2000 1 Time.Mon
            , dateOfRegistration = Date.fromWeekDate 2020 10 Time.Tue
            , email = "test@email.com"
            , gender = Utils.Male
            , school = "Test school"
            , status = Tutor.Active
            , password = Nothing
            }
    in
    test "Decode and encode tutor json"
        (\_ ->
            tutor
                |> Tutor.tutorEncoder
                |> Json.Encode.encode 2
                |> Json.Decode.decodeString Tutor.tutorDecoder
                |> Expect.all
                    [ Expect.ok
                    , Result.toMaybe >> Maybe.withDefault Tutor.emptyTutor >> Expect.equal tutor
                    ]
        )
