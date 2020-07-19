module Youtube.PlayerState exposing
    ( PlayerState(..)
    , decoder
    , mapPlayerState
    )

import Json.Decode as Decode


type PlayerState
    = Unstarted
    | Ended
    | Playing
    | Paused
    | Buffering
    | Queued


mapPlayerState : Int -> Maybe PlayerState
mapPlayerState id =
    case String.fromInt id of
        "-1" -> Just Unstarted
        "0" -> Just Ended
        "1" -> Just Playing
        "2" -> Just Paused
        "3" -> Just Buffering
        "5" -> Just Queued
        _ -> Nothing


decoder : Decode.Decoder PlayerState
decoder =
    Decode.andThen
        (\val ->
            case mapPlayerState val of
                Just state ->
                    Decode.succeed state

                Nothing ->
                    Decode.fail <| "Unexpected value '" ++ (String.fromInt val) ++ "'."
        )
        Decode.int
