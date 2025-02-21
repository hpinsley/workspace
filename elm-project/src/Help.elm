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
          displayHelpText model
        , displayHelpActions model
    ]

displayHelpActions: Model -> Html Msg
displayHelpActions model =
    div [id "help-actions"][
        Button.text (Button.config |> Button.setOnClick (ShowHelp False)) "Close Help"
    ]

displayHelpText: Model -> Html Msg
displayHelpText model =
    div [id "help-text"]
    [
        text """
            This application allows you to enter a mathematical equation and plot the results.
            Most common math functions are supported (e.g. sin, cos, tan, ln, abs).
            
            If you enter
            a one-variable function (generally with independent variable lowercase x) you will get a
            2-dimensional plot with an x and y axis.alias.alias

            Sample: You enter sin(x).  This will create a 2-dimensional plot (assuming a y axis as the representation of the dependent variable)
        ]
            If you enter
            """
    ]