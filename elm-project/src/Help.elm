module Help exposing (displayHelp)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material.Button as Button
import Models exposing (..)
import Utils


displayHelp: Model -> Html Msg
displayHelp model =
    div [id "help-page"][
        text "Hey! Enter a mathematical equation.  I can plot functions of either one (e.g of x) or two (e.g. of x and y) independent variables."
        , displayHelpActions model
    ]

displayHelpActions: Model -> Html Msg
displayHelpActions model =
    div [id "help-actions"][
        button [onClick (ShowHelp False)][text "Close Help"]
    ]
