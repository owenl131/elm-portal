module Class exposing (..)

import Date

type alias Class = 
    { id : Int 
    , name : String 
    , day : Date.Weekday
    , timeslot : String
    , duration : Float
    }

type alias ClassExtended =  
    { class : Class
    , sessions : List ClassSession
    , tutors : List ClassTutor 
    }

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