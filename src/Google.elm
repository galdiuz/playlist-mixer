module Google exposing
    ( oauthUrl
    , oauthScopeYoutube
    , oauthScopeYoutubeReadOnly
    , tokenHasWriteScope
    )

import Url exposing (Url)

import App


oauthUrl : Url
oauthUrl =
    { emptyUrl
        | host = "accounts.google.com"
        , path = "/o/oauth2/v2/auth"
    }


emptyUrl : Url
emptyUrl =
    { protocol = Url.Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


oauthScopeYoutube : String
oauthScopeYoutube =
    "https://www.googleapis.com/auth/youtube"


oauthScopeYoutubeReadOnly : String
oauthScopeYoutubeReadOnly =
    "https://www.googleapis.com/auth/youtube.readonly"


tokenHasWriteScope : Maybe App.Token -> Bool
tokenHasWriteScope maybeToken =
    case maybeToken of
        Just token ->
            List.member oauthScopeYoutube token.scopes

        Nothing ->
            False
