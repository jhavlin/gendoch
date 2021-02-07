module Components.Common exposing (ConfirmDeleteButtonConfig, confirmDeleteButton)

import Html exposing (Attribute, Html, button, div, text)
import Html.Events exposing (onClick)


type alias ConfirmDeleteButtonConfig msg =
    { isBeingDeleted : Bool
    , startText : String
    , questionText : String
    , confirmText : String
    , startMsg : msg
    , confirmMsg : msg
    , cancelText : String
    , cancelMsg : msg
    , attrs : List (Attribute msg)
    }


confirmDeleteButton : ConfirmDeleteButtonConfig msg -> Html msg
confirmDeleteButton { isBeingDeleted, startText, questionText, confirmText, startMsg, confirmMsg, cancelText, cancelMsg, attrs } =
    if isBeingDeleted then
        div attrs
            [ text questionText
            , text " "
            , button [ onClick confirmMsg ] [ text confirmText ]
            , button [ onClick cancelMsg ] [ text cancelText ]
            ]

    else
        div attrs
            [ button [ onClick startMsg ] [ text startText ]
            ]
