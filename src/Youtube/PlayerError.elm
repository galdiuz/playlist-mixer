module Youtube.PlayerError exposing
    ( PlayerError(..)
    , decoder
    , mapPlayerError
    )

import Json.Decode as Decode


type PlayerError
    = InvalidParam
    | Html5Error
    | NotFound
    | NoEmbed


mapPlayerError : Int -> Maybe PlayerError
mapPlayerError id =
    case id of
        2 -> Just InvalidParam
        5 -> Just Html5Error
        100 -> Just NotFound
        101 -> Just NoEmbed
        150 -> Just NoEmbed
        _ -> Nothing


decoder : Decode.Decoder PlayerError
decoder =
    Decode.andThen
        (\val ->
            case mapPlayerError val of
                Just state ->
                    Decode.succeed state

                Nothing ->
                    Decode.fail <| "Unexpected value '" ++ (String.fromInt val) ++ "'."
        )
        Decode.int
