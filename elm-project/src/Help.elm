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
          displayHelpActions model
        , displayHelpText model
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
          img [src "assets/sample-3d-input.png"][]
        , p [][text "This application allows you to enter a mathematical equation and plot the results.  Most common math functions are supported."]
        , div [class "code-block"] [text "(e.g. sin, cos, tan, ln, abs)"]
        , h2 [][text "One Variable Functions"]
        , text "If you enter a one-variable function (generally with independent variable lowercase x) you will get a 2-dimensional plot with an x and y axis"
        , text "Here is an example:"
        , div [class "code-block"][text "sin(x)"]
        , text "This will create a 2-dimensional plot (assuming a y axis as the representation of the dependent variable)."
        , h2 [][text "Two Variable Functions"]
    ]