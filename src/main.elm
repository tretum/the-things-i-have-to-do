module Main exposing (Model, Msg(..), TodoEntry, init, main, update, view)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias TodoEntry =
    { id : Int, text : String, done : Bool, edit : Maybe String }


type alias Model =
    { listName : String
    , entries : List TodoEntry
    , maxIndex : Int
    , currentInput : String
    , currentDeleteID : String
    }


init : Model
init =
    Model "MyList" [] 0 "" ""



-- UPDATE


type Msg
    = AddEntry String
    | AddEntryButtonPressed
    | EntryMarkedCompleted Int
    | EntryMarkedNotDone Int
    | DeleteEntry
    | DeleteEntryById Int
    | DeleteAllEntries
    | GetText String
    | UpdateDeleteIdString String
    | ApplyEdit Int
    | UpdateEdit Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddEntry text ->
            let
                newIndex =
                    model.maxIndex + 1
            in
            { model
                | maxIndex = newIndex
                , entries = model.entries ++ [ TodoEntry newIndex text False Nothing ]
                , currentInput = ""
            }

        AddEntryButtonPressed ->
            update (AddEntry model.currentInput) model

        EntryMarkedCompleted id ->
            model

        EntryMarkedNotDone id ->
            model

        DeleteAllEntries ->
            { model | entries = [] }

        DeleteEntryById id ->
            deleteEntry model id

        DeleteEntry ->
            case String.toInt model.currentDeleteID of
                Just id ->
                    let
                        newModel =
                            deleteEntry model id
                    in
                    { newModel | currentDeleteID = "" }

                Nothing ->
                    { model | currentDeleteID = "" }

        UpdateDeleteIdString id ->
            { model | currentDeleteID = id }

        GetText text ->
            { model | currentInput = text }

        UpdateEdit id text ->
            let
                updateEdit entry =
                    if entry.id == id then
                        { entry | edit = Just text }

                    else
                        entry
            in
            { model | entries = List.map updateEdit model.entries }

        ApplyEdit id ->
            let
                applyEdit entry =
                    if entry.id == id then
                        case entry.edit of
                            Just updateText ->
                                { entry | text = updateText, edit = Nothing }

                            Nothing ->
                                { entry | text = "" }

                    else
                        entry
            in
            { model | entries = List.map applyEdit model.entries }


deleteEntry : Model -> Int -> Model
deleteEntry model id =
    { model
        | entries = List.filter (.id >> (/=) id) model.entries
    }



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "text-align" "center" ]
        [ Html.header [] [ text model.listName ]
        , viewInput model.currentInput
        , viewDeleteItem model.currentDeleteID
        , viewEntries model
        , button [ onClick DeleteAllEntries ] [ text "Delete all entries" ]
        ]


{-| An input form for the new todo.

Exploits the browser's understanding of forms and submits.
Automatically fires the onSubmit event when the submit input is pressed
as well as when enter is typed in the text input.

-}
viewInput : String -> Html Msg
viewInput current =
    Html.form [ onSubmit AddEntryButtonPressed ]
        [ input [ placeholder "Enter your new input", value current, onInput GetText, autofocus True ] []
        , input [ type_ "submit", value "+" ] []
        ]


viewEntries : Model -> Html Msg
viewEntries model =
    div []
        (List.map viewEntry model.entries)


viewEntry : TodoEntry -> Html Msg
viewEntry todoEntry =
    case todoEntry.edit of
        Just editText ->
            div []
                [ Html.form [ onSubmit (ApplyEdit todoEntry.id) ]
                    [ input
                        [ placeholder "New value"
                        , value editText
                        , onInput (UpdateEdit todoEntry.id)
                        , autofocus True
                        ]
                        []
                    , input [ type_ "submit", value "Save" ] []
                    ]
                , button [ onClick (DeleteEntryById todoEntry.id) ] [ text "x" ]
                ]

        Nothing ->
            div []
                [ text (String.fromInt todoEntry.id ++ ": ")
                , text todoEntry.text
                , button [ onClick (UpdateEdit todoEntry.id todoEntry.text) ] [ text "Edit" ]
                , button [ onClick (DeleteEntryById todoEntry.id) ] [ text "x" ]
                ]


viewDeleteItem : String -> Html Msg
viewDeleteItem currentID =
    Html.form [ onSubmit DeleteEntry ]
        [ input
            [ placeholder "Enter id of item to delete"
            , value currentID
            , onInput UpdateDeleteIdString
            ]
            []
        , input [ type_ "submit", value "Delete specified entry" ] []
        ]
