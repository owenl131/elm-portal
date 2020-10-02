module Api exposing (Credentials)

import Time


type alias Credentials =
    { email : String
    , session : String
    , sessionExpiry : Time.Posix
    }
