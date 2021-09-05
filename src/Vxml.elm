module Vxml exposing (BlockChild(..), FormChild(..), Vxml(..), VxmlChild(..), decoder)

import Vxml.XmlDataTypes exposing (CData, Duration, Expression, NMTOKEN, RestrictedVariableName, URI)
import Xml.Decode
import XmlParser exposing (Node(..))


type Vxml
    = Vxml
        { application : Maybe URI
        , base : Maybe URI
        , lang : Maybe NMTOKEN
        , xmlns : CData
        , xsi : Maybe CData
        , schemaLocation : Maybe CData
        , version : CData
        }
        (List VxmlChild)


type VxmlChild
    = Form {} (List FormChild)
    | Meta
        { name : Maybe String
        , content : String
        , httpEquiv : Maybe String
        }
    | Metadata {}
    | Script {} CData
    | Var
        { name : RestrictedVariableName
        , expr : Maybe Expression
        }
      --| Property
      --| Data
      --| Catch
      --| Help
      --| NoInput
      --| NoMatch
      --| Error
      --| Link
      --| Menu
    | VxmlWhiteSpace


type FormChild
    = Block {} (List BlockChild)
    | FormWhiteSpace


type BlockChild
    = BlockCData CData
    | Value
        { expr : Expression
        }
    | Goto
        { fetchtimeout : Maybe Duration
        , maxage : Maybe Int
        , maxstale : Maybe Int
        , fetchhint : Maybe Duration
        , next : Maybe URI
        , expr : Maybe Expression
        , fetchaudio : Maybe URI
        , expritem : Maybe Expression
        , nextitem : Maybe RestrictedVariableName
        }
    | BlockWhiteSpace


decoder : Xml.Decode.Decoder Vxml
decoder =
    Xml.Decode.succeed
        (\application base lang xmlns xsi schemaLocation version children ->
            Vxml
                { application = application
                , base = base
                , lang = lang
                , xmlns = xmlns
                , xsi = xsi
                , schemaLocation = schemaLocation
                , version = version
                }
                children
        )
        |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "application")
        |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "base")
        |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "lang")
        |> Xml.Decode.andMap
            (Xml.Decode.stringAttr "xmlns"
                |> Xml.Decode.andThen
                    (\xmlns ->
                        if xmlns == "http://www.w3.org/2001/vxml" then
                            Xml.Decode.succeed xmlns

                        else
                            Xml.Decode.fail ("Expected xmlns is http://www.w3.org/2001/vxml but found " ++ xmlns)
                    )
            )
        |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "xsi")
        |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "schemaLocation")
        |> Xml.Decode.andMap
            (Xml.Decode.stringAttr "version"
                |> Xml.Decode.andThen
                    (\version ->
                        if not (version == "2.0") then
                            Xml.Decode.fail ("Found version " ++ version ++ "but only supports 2.0")

                        else
                            Xml.Decode.succeed version
                    )
            )
        |> Xml.Decode.andMap
            (Xml.Decode.path []
                (Xml.Decode.list
                    (Xml.Decode.oneOf
                        [ whiteSpaceDecoder (\() -> VxmlWhiteSpace)
                        , metaDecoder
                        , varDecoder
                        , formDecoder
                        , scriptDecoder
                        , metadataDecoder
                        ]
                    )
                )
                |> Xml.Decode.map (List.filter ((/=) VxmlWhiteSpace))
            )


whiteSpaceDecoder : (() -> a) -> Xml.Decode.Decoder a
whiteSpaceDecoder f =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\node ->
                case node of
                    Text _ ->
                        decodeAsWhiteSpace f

                    _ ->
                        Xml.Decode.fail "Not whitespace"
            )


decodeAsWhiteSpace : (() -> a) -> Xml.Decode.Decoder a
decodeAsWhiteSpace f =
    Xml.Decode.string
        |> Xml.Decode.andThen
            (\s ->
                if
                    s
                        |> String.toList
                        |> List.any (\c -> c /= '\n' && c /= '\u{000D}' && c /= '\t' && c /= ' ')
                then
                    Xml.Decode.fail "Not whitespace"

                else
                    Xml.Decode.succeed (f ())
            )


decodeAsForm : Xml.Decode.Decoder VxmlChild
decodeAsForm =
    Xml.Decode.map (Form {})
        (Xml.Decode.path []
            (Xml.Decode.list
                (Xml.Decode.oneOf
                    [ whiteSpaceDecoder (\() -> FormWhiteSpace)
                    , blockDecoder
                    ]
                )
            )
            |> Xml.Decode.map (List.filter ((/=) FormWhiteSpace))
        )


formDecoder : Xml.Decode.Decoder VxmlChild
formDecoder =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\element ->
                case element of
                    Element "form" _ _ ->
                        decodeAsForm

                    _ ->
                        Xml.Decode.fail "Not a form element"
            )


metadataDecoder : Xml.Decode.Decoder VxmlChild
metadataDecoder =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\element ->
                case element of
                    Element "metadata" _ _ ->
                        Xml.Decode.succeed (Metadata {})

                    _ ->
                        Xml.Decode.fail "Not a metadata element"
            )


metaDecoder : Xml.Decode.Decoder VxmlChild
metaDecoder =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\node ->
                case node of
                    Element "meta" _ _ ->
                        Xml.Decode.map3 (\name content httpEquiv -> Meta { name = name, content = content, httpEquiv = httpEquiv })
                            (Xml.Decode.maybe <| Xml.Decode.stringAttr "name")
                            (Xml.Decode.stringAttr "content")
                            (Xml.Decode.maybe <| Xml.Decode.stringAttr "http-equiv")

                    _ ->
                        Xml.Decode.fail "Not a meta element"
            )


varDecoder : Xml.Decode.Decoder VxmlChild
varDecoder =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\node ->
                case node of
                    Element "var" _ _ ->
                        Xml.Decode.map2 (\name expr -> Var { name = name, expr = expr })
                            (Xml.Decode.stringAttr "name")
                            (Xml.Decode.maybe <| Xml.Decode.stringAttr "expr")

                    _ ->
                        Xml.Decode.fail "Not a var element"
            )


gotoDecoder : Xml.Decode.Decoder BlockChild
gotoDecoder =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\node ->
                case node of
                    Element "goto" _ _ ->
                        Xml.Decode.succeed
                            (\fetchtimeout fetchhint maxage maxstale next expr fetchaudio expritem nextitem ->
                                Goto
                                    { fetchtimeout = fetchtimeout
                                    , fetchhint = fetchhint
                                    , maxage = maxage
                                    , maxstale = maxstale
                                    , next = next
                                    , expr = expr
                                    , fetchaudio = fetchaudio
                                    , expritem = expritem
                                    , nextitem = nextitem
                                    }
                            )
                            |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "fetchtimeout")
                            |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "fetchhint")
                            |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.intAttr "maxage")
                            |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.intAttr "maxstale")
                            |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "next")
                            |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "expr")
                            |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "fetchaudio")
                            |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "expritem")
                            |> Xml.Decode.andMap (Xml.Decode.maybe <| Xml.Decode.stringAttr "nextitem")

                    _ ->
                        Xml.Decode.fail "Not a goto element"
            )


scriptDecoder : Xml.Decode.Decoder VxmlChild
scriptDecoder =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\element ->
                case element of
                    Element "script" _ _ ->
                        Xml.Decode.string
                            |> Xml.Decode.map (Script {})

                    _ ->
                        Xml.Decode.fail "Not a script element"
            )


blockDecoder : Xml.Decode.Decoder FormChild
blockDecoder =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\element ->
                case element of
                    Element "block" _ _ ->
                        decodeAsBlock

                    _ ->
                        Xml.Decode.fail "Not a block element"
            )


decodeAsBlock : Xml.Decode.Decoder FormChild
decodeAsBlock =
    Xml.Decode.map (Block {})
        (Xml.Decode.path []
            (Xml.Decode.list
                (Xml.Decode.oneOf
                    [ whiteSpaceDecoder (\() -> BlockWhiteSpace)
                    , valueDecoder
                    , gotoDecoder
                    , cDataDecoder BlockCData
                    ]
                )
            )
            |> Xml.Decode.map (List.filter ((/=) BlockWhiteSpace))
        )


valueDecoder : Xml.Decode.Decoder BlockChild
valueDecoder =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\node ->
                case node of
                    Element "value" _ _ ->
                        Xml.Decode.map (\expr -> Value { expr = expr })
                            (Xml.Decode.stringAttr "expr")

                    _ ->
                        Xml.Decode.fail "Not a value element"
            )


cDataDecoder : (String -> a) -> Xml.Decode.Decoder a
cDataDecoder f =
    Xml.Decode.node
        |> Xml.Decode.andThen
            (\node ->
                case node of
                    Text "" ->
                        Xml.Decode.fail "Empty text node"

                    Text _ ->
                        Xml.Decode.string
                            |> Xml.Decode.map f

                    _ ->
                        Xml.Decode.fail "Not a text node"
            )
