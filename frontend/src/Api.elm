module Api exposing (Credentials, endpoint)

import Time


endpoint : String
endpoint =
    "http://localhost:8001/backend"


type alias Credentials =
    { email : String
    , session : String
    , sessionExpiry : Time.Posix
    }
