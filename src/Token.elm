module Token exposing (Token, decoder, encode, named)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Token a
    = Token String a



-- CREATING


named : String -> a -> Token a
named =
    Token



-- ENCODE


encode : (a -> Value) -> Token a -> Value
encode encodeDefault (Token name default) =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "default", encodeDefault default )
        ]



-- DECODE


decoder : Decoder a -> Decoder (Token a)
decoder defaultDecoder =
    Decode.map2 Token
        (Decode.field "name" Decode.string)
        (Decode.field "default" defaultDecoder)
