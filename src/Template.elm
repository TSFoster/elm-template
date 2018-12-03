module Template exposing (Component(..), Template, decoder, empty, encode, fromComponents, map, remainingTokens, replaceToken, swapToken, toComponents, toString)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode



-- Why `(String, List ( token, String ))` and not just `List
-- Component`? Equality. `fromComponents [ Static "Hello ", Static "World" ]`
-- shold be equal to `fromComponents [ Static "Hello world" ]`.


type Template token
    = Template ( String, List ( token, String ) )


type Component token
    = Token token
    | Static String



-- COMPOSING


empty : Template token
empty =
    Template ( "", [] )


fromComponents : List (Component token) -> Template token
fromComponents =
    List.foldr fromComponentsFoldr empty


fromComponentsFoldr : Component token -> Template token -> Template token
fromComponentsFoldr component (Template ( initial, components )) =
    case ( component, components ) of
        ( Static static, [] ) ->
            Template ( initial ++ static, [] )

        ( Static static, ( prevToken, prevStatic ) :: rest ) ->
            Template ( initial, ( prevToken, prevStatic ++ static ) :: rest )

        ( Token token, _ ) ->
            Template ( initial, ( token, "" ) :: components )


toComponents : Template token -> List (Component token)
toComponents (Template ( initial, components )) =
    Static initial :: List.concatMap toComponent components


toComponent : ( token, String ) -> List (Component token)
toComponent ( token, string ) =
    if string == "" then
        [ Token token ]

    else
        [ Token token, Static string ]


map : (token1 -> token2) -> Template token1 -> Template token2
map fn (Template ( intitial, components )) =
    Template ( intitial, List.map (Tuple.mapFirst fn) components )



-- DEALING WITH TOKENS


remainingTokens : Template token -> List token
remainingTokens (Template ( _, components )) =
    components
        |> List.map Tuple.first
        |> List.foldr consIfNew []


consIfNew : a -> List a -> List a
consIfNew item items =
    if List.member item items then
        items

    else
        item :: items


replaceToken : token -> String -> Template token -> Template token
replaceToken toReplace replacement (Template ( initial, components )) =
    case components of
        [] ->
            Template ( initial, components )

        ( token, string ) :: rest ->
            if token == toReplace then
                Template ( initial ++ replacement ++ string, rest )
                    |> replaceToken toReplace replacement

            else
                rest
                    |> List.foldl (replaceFold toReplace replacement) ( ( token, string ), [] )
                    |> uncurry (::)
                    |> List.reverse
                    |> Tuple.pair initial
                    |> Template


replaceFold : token -> String -> ( token, String ) -> ( ( token, String ), List ( token, String ) ) -> ( ( token, String ), List ( token, String ) )
replaceFold toReplace replacement ( token, string ) ( ( prevToken, prevString ), newComponents ) =
    if token == toReplace then
        ( ( prevToken, prevString ++ replacement ++ string ), newComponents )

    else
        ( ( token, string ), ( prevToken, prevString ) :: newComponents )


toString : Template String -> String
toString (Template ( initial, components )) =
    components
        |> List.map (uncurry (++))
        |> String.concat
        |> String.append initial


swapToken : token -> token -> Template token -> Template token
swapToken old =
    map << swapToIf old


swapToIf : a -> a -> a -> a
swapToIf old new thing =
    if thing == old then
        new

    else
        thing



-- ENCODE


encode : (token -> Value) -> Template token -> Value
encode encodeToken (Template ( initial, components )) =
    components
        |> List.concatMap (encodeComponent encodeToken)
        |> List.append (encodeString initial)
        |> Encode.list identity


encodeComponent : (token -> Value) -> ( token, String ) -> List Value
encodeComponent encodeToken ( token, string ) =
    Encode.object [ ( "token", encodeToken token ) ] :: encodeString string


encodeString : String -> List Value
encodeString string =
    if string == "" then
        []

    else
        [ Encode.string string ]



-- DECODE


decoder : Decoder token -> Decoder (Template token)
decoder tokenDecoder =
    [ Decode.map Token (Decode.field "token" tokenDecoder)
    , Decode.map Static Decode.string
    ]
        |> Decode.oneOf
        |> Decode.list
        |> Decode.map fromComponents



-- COMMON


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b
