module Token exposing (Token, decoder, default, encode, map, name, named)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Token a
    = Token String a



-- CREATING


named : String -> a -> Token a
named =
    Token


map : (a -> b) -> Token a -> Token b
map fn (Token theName theDefault) =
    Token theName (fn theDefault)



-- INSPECTING


name : Token a -> String
name (Token theName _) =
    theName


default : Token a -> a
default (Token _ theDefault) =
    theDefault



-- ENCODE


encode : (a -> Value) -> Token a -> Value
encode encodeDefault (Token theName theDefault) =
    Encode.object
        [ ( "name", Encode.string theName )
        , ( "default", encodeDefault theDefault )
        ]



-- DECODE


decoder : Decoder a -> Decoder (Token a)
decoder defaultDecoder =
    Decode.map2 Token
        (Decode.field "name" Decode.string)
        (Decode.field "default" defaultDecoder)
