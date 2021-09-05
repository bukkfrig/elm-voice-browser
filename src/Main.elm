module Main exposing (main)

import Browser
import Html exposing (text)
import Task
import Vxml exposing (Vxml(..))
import Vxml.Application exposing (Application(..))
import Vxml.Document exposing (Document(..))
import Xml.Decode
import XmlParser exposing (Node(..))


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , subscriptions = \_ -> Sub.none
        , init =
            \() ->
                ( Loading
                , Task.perform Parsed
                    (Task.succeed
                        (Xml.Decode.run Vxml.decoder testVxml)
                    )
                )
        , update =
            \msg model ->
                case msg of
                    Parsed (Ok vxml) ->
                        case Vxml.Application.fromVxml { uri = "blah", vxml = vxml } of
                            Ok application ->
                                ( Loaded vxml (Vxml.Application.initialise application)
                                , Cmd.none
                                )

                            Err err ->
                                ( Failed err
                                , Cmd.none
                                )

                    Parsed (Err err) ->
                        ( Failed err
                        , Cmd.none
                        )

                    Tick ->
                        case model of
                            Loaded vxml application ->
                                let
                                    { appstate, cmd } =
                                        Vxml.Application.step application
                                in
                                ( Loaded vxml appstate, Cmd.map ApplicationMsg cmd )

                            _ ->
                                Debug.todo "Use a better model"

                    ApplicationMsg appMsg ->
                        Debug.todo ("Should handle " ++ Debug.toString appMsg)
        }


type Msg
    = Parsed (Result String Vxml)
    | Tick
    | ApplicationMsg Vxml.Application.Msg


type Model
    = Loading
    | Loaded Vxml Application
    | Failed String


view : Model -> Html.Html msg
view model =
    case model of
        Loading ->
            text "Loading..."

        Loaded _ _ ->
            text "Check the Elm debugger for a view of the parsed VXML"

        Failed error ->
            text error


testVxml : String
testVxml =
    """
    <?xml version="1.0" encoding="UTF-8"?>
<vxml xmlns="http://www.w3.org/2001/vxml" 
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
  xsi:schemaLocation="http://www.w3.org/2001/vxml 
   http://www.w3.org/TR/voicexml20/vxml.xsd"
   version="2.0">
 <meta name="author" content="John Doe"/>
 <meta name="maintainer" content="hello-support@hi.example.com"/>
 <var name="hi" expr="'Hello World!'"/>
 <form>
  <block>
     <value expr="hi"/>
     <goto next="#say_goodbye"/>
  </block>
 </form>
 <form id="say_goodbye">
  <block>
     Goodbye!
  </block>
 </form>
</vxml>
    """
