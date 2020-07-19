module Youtube.Page exposing
    ( Page
    , decoder
    )

import Json.Decode as Decode
import Json.Decode.Field as Field


type alias Page a =
    { total : Int
    , perPage : Int
    , nextPageToken : Maybe String
    , prevPageToken : Maybe String
    , items : List a
    }


decoder : Decode.Decoder a -> Decode.Decoder (Page a)
decoder itemsDecoder =
    Field.requireAt [ "pageInfo", "totalResults" ] Decode.int <| \total ->
    Field.requireAt [ "pageInfo", "resultsPerPage" ] Decode.int <| \perPage ->
    Field.attempt "nextPageToken" Decode.string <| \nextPageToken ->
    Field.attempt "prevPageToken" Decode.string <| \prevPageToken ->
    Field.require "items" (Decode.list itemsDecoder) <| \items ->
    Decode.succeed
        { total = total
        , perPage = perPage
        , nextPageToken = nextPageToken
        , prevPageToken = prevPageToken
        , items = items
        }
