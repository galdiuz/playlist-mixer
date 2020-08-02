module Google.OAuth exposing
    ( url
    , tokenHasReadScope
    , tokenHasWriteScope
    )

import Url exposing (Url)

import App
import Google.OAuth.Scope


url : Url
url =
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


tokenHasReadScope : Maybe App.Token -> Bool
tokenHasReadScope maybeToken =
    case maybeToken of
        Just token ->
            List.member Google.OAuth.Scope.Youtube token.scopes
                || List.member Google.OAuth.Scope.YoutubeReadOnly token.scopes

        Nothing ->
            False


tokenHasWriteScope : Maybe App.Token -> Bool
tokenHasWriteScope maybeToken =
    case maybeToken of
        Just token ->
            List.member Google.OAuth.Scope.Youtube token.scopes

        Nothing ->
            False
