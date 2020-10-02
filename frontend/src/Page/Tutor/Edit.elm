module Page.Tutor.Edit exposing (Model)

import Api
import Browser.Navigation as Navigation


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    }
