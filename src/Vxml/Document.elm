module Vxml.Document exposing (Dialog(..), Document(..), FormInitialiser, FormItem, Initialiser, fromVxml)

import Vxml exposing (Vxml(..), VxmlChild)
import Vxml.XmlDataTypes exposing (CData, Event, Expression, RestrictedVariableName, URI)


type Document
    = Document
        { application : Maybe URI
        , uri : URI
        , initialisers : List Initialiser
        , firstDialog : Dialog
        , otherDialogs : List Dialog
        , eventHandlers : List EventHandler
        }


type Initialiser
    = InitialiserScript {} CData
    | InitialiserVar
        { name : RestrictedVariableName
        , expr : Maybe Expression
        }


type Dialog
    = DialogForm
        { initialisers : List FormInitialiser
        , firstFormItem : FormItem
        , otherFormItems : List FormItem
        }
    | DialogMenu


type FormItem
    = FormItemBlock { exectuables : List Executable }


type FormInitialiser
    = FormInitialiserObject
    | FormInitialiserVar


type Executable
    = ExecutableVar
    | ExecutableValue
    | Assign
    | Clear
    | If
    | ElseIf
    | Else
    | Prompt
    | Reprompt
    | ExecutableGoto
    | Submit
    | Exit
    | Return
    | Disconnect
    | ExecutableScript
    | Log


type EventHandler
    = EventHandler Event (List Executable)


fromVxml : { uri : URI, vxml : Vxml } -> Result String Document
fromVxml { uri, vxml } =
    case vxml of
        Vxml attrs children ->
            case parseVxmlChildren children of
                Ok { initialisers, dialogs, eventHandlers } ->
                    case dialogs of
                        x :: xs ->
                            Ok <|
                                Document
                                    { application = attrs.application
                                    , uri = uri
                                    , initialisers = initialisers
                                    , firstDialog = x
                                    , otherDialogs = xs
                                    , eventHandlers = eventHandlers
                                    }

                        [] ->
                            Err "Vxml has no dialogs"

                Err err ->
                    Err err


parseVxmlChildren : List VxmlChild -> Result String { initialisers : List Initialiser, dialogs : List Dialog, eventHandlers : List EventHandler }
parseVxmlChildren children =
    children
        |> List.foldl
            (\child accum ->
                case accum of
                    Err err ->
                        Err err

                    Ok acc ->
                        case child of
                            Vxml.Form attrs formChildren ->
                                case dialogFormFromVxmlForm attrs formChildren of
                                    Ok dialog ->
                                        Ok { acc | dialogs = acc.dialogs ++ [ dialog ] }

                                    Err err ->
                                        Err err

                            Vxml.Meta _ ->
                                Ok acc

                            Vxml.Metadata attrs ->
                                Ok acc

                            Vxml.Script attrs cdata ->
                                Ok { acc | initialisers = acc.initialisers ++ [ InitialiserScript attrs cdata ] }

                            Vxml.Var { name, expr } ->
                                Ok { acc | initialisers = acc.initialisers ++ [ InitialiserVar { name = name, expr = expr } ] }

                            Vxml.VxmlWhiteSpace ->
                                Ok acc
            )
            (Ok
                { initialisers = []
                , dialogs = []
                , eventHandlers = []
                }
            )


dialogFormFromVxmlForm : {} -> List Vxml.FormChild -> Result String Dialog
dialogFormFromVxmlForm attrs children =
    let
        { initialisers, formItems } =
            parseFormChildren children
    in
    case formItems of
        x :: xs ->
            Ok
                (DialogForm
                    { initialisers = initialisers
                    , firstFormItem = x
                    , otherFormItems = xs
                    }
                )

        [] ->
            Err "Form has no FormItems"


parseFormChildren : List Vxml.FormChild -> { initialisers : List FormInitialiser, formItems : List FormItem }
parseFormChildren children =
    children
        |> List.foldl
            (\child acc ->
                case child of
                    Vxml.Block attrs blockChildren ->
                        { acc | formItems = acc.formItems ++ [ formItemBlockFromVxmlBlock attrs blockChildren ] }

                    Vxml.FormWhiteSpace ->
                        acc
            )
            { initialisers = []
            , formItems = []
            }


formItemBlockFromVxmlBlock : {} -> List Vxml.BlockChild -> FormItem
formItemBlockFromVxmlBlock attrs children =
    let
        { executables } =
            parseBlockChildren children
    in
    FormItemBlock
        { exectuables = executables }


parseBlockChildren : List Vxml.BlockChild -> { executables : List Executable }
parseBlockChildren children =
    children
        |> List.foldl
            (\child acc ->
                case child of
                    Vxml.BlockCData cdata ->
                        -- TODO: Should be its own type instead of a Prompt?
                        { acc | executables = acc.executables ++ [ Prompt ] }

                    Vxml.Value attrs ->
                        { acc | executables = acc.executables ++ [ ExecutableValue ] }

                    Vxml.Goto attrs ->
                        { acc | executables = acc.executables ++ [ ExecutableGoto ] }

                    Vxml.BlockWhiteSpace ->
                        acc
            )
            { executables = [] }
