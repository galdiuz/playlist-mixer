module Youtube.PlayerError exposing
    ( PlayerError(..)
    , mapPlayerError
    )


type PlayerError
    = InvalidParam
    | Html5Error
    | NotFound
    | NoEmbedded


mapPlayerError : Int -> Maybe PlayerError
mapPlayerError id =
    case id of
        2 -> Just InvalidParam
        5 -> Just Html5Error
        100 -> Just NotFound
        101 -> Just NoEmbedded
        _ -> Nothing
