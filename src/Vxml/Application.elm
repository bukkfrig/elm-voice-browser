module Vxml.Application exposing (Application(..), Msg(..), State(..), fromVxml, initialise, step)

import Json.Encode
import List.Zipper as Zipper exposing (Zipper)
import Vxml exposing (Vxml)
import Vxml.Document exposing (Dialog(..), Document(..))
import Vxml.XmlDataTypes exposing (URI)


type Application
    = Application
        { rootDocument : Maybe Document
        , activeDocument : Document
        , state : State
        , context : Json.Encode.Value
        }


type State
    = New
    | Initialising (Zipper Vxml.Document.Initialiser)
    | InitialisingForm { dialogs : Zipper Vxml.Document.Dialog, formInitialisers : Zipper Vxml.Document.FormInitialiser }
    | ExecutingForm { dialogs : Zipper Vxml.Document.Dialog, formItems : Zipper Vxml.Document.FormItem }
    | ExecutingMenu { dialogs : Zipper Vxml.Document.Dialog }


fromVxml : { uri : URI, vxml : Vxml } -> Result String Application
fromVxml { uri, vxml } =
    case Vxml.Document.fromVxml { uri = uri, vxml = vxml } of
        Ok activeDocument ->
            Ok <|
                Application
                    { rootDocument = Nothing
                    , activeDocument = activeDocument
                    , state = New
                    , context = Json.Encode.object []
                    }

        Err err ->
            Err err


type Msg
    = ExecutedInitialiser


step : Application -> { appstate : Application, cmd : Cmd Msg }
step ((Application { rootDocument, activeDocument, state, context }) as app) =
    case state of
        New ->
            { appstate = initialise app, cmd = Cmd.none }

        Initialising initialiserZipper ->
            let
                thisCmd =
                    executeInitialiser (\() -> ExecutedInitialiser) (Zipper.current initialiserZipper)

                activeDocumentProperties =
                    case activeDocument of
                        Document docProperties ->
                            docProperties

                nextState =
                    case Zipper.next initialiserZipper of
                        Just nextZipper ->
                            Application { rootDocument = rootDocument, activeDocument = activeDocument, state = Initialising nextZipper, context = context }

                        Nothing ->
                            case activeDocumentProperties.firstDialog of
                                Vxml.Document.DialogForm { initialisers } ->
                                    case initialisers of
                                        x :: xs ->
                                            Application
                                                { rootDocument = rootDocument
                                                , activeDocument = activeDocument
                                                , context = context
                                                , state =
                                                    InitialisingForm
                                                        { dialogs = Zipper.fromCons activeDocumentProperties.firstDialog activeDocumentProperties.otherDialogs
                                                        , formInitialisers = Zipper.fromCons x xs
                                                        }
                                                }

                                        [] ->
                                            executeDialogs app

                                DialogMenu ->
                                    Debug.todo "menu..."
            in
            { appstate = nextState, cmd = thisCmd }

        InitialisingForm { dialogs, formInitialisers } ->
            Debug.todo "duh"

        ExecutingForm { dialogs, formItems } ->
            Debug.todo "duh"

        ExecutingMenu _ ->
            Debug.todo "duh"


executeInitialiser : (() -> msg) -> Vxml.Document.Initialiser -> Cmd msg
executeInitialiser toMsg initialiser =
    Debug.todo "duh"


initialise : Application -> Application
initialise ((Application ({ rootDocument, activeDocument, state } as attrs)) as app) =
    case activeDocument of
        Document { initialisers } ->
            case initialisers of
                x :: xs ->
                    Application
                        { attrs | state = Initialising (Zipper.fromCons x xs) }

                [] ->
                    executeDialogs app


executeDialogs : Application -> Application
executeDialogs (Application ({ activeDocument } as attrs)) =
    case activeDocument of
        Document { firstDialog, otherDialogs } ->
            case firstDialog of
                DialogForm { firstFormItem, otherFormItems } ->
                    Application
                        { attrs
                            | state =
                                ExecutingForm
                                    { dialogs = Zipper.fromCons firstDialog otherDialogs
                                    , formItems = Zipper.fromCons firstFormItem otherFormItems
                                    }
                        }

                DialogMenu ->
                    Debug.todo "ExecutingMenu"
