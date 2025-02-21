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
          h1 [][text "Help"]
        , p [][text "This application allows you to enter a mathematical equation and plot the results.  Most common math functions are supported."]
        , div [class "code-block"] [text "(e.g. sin, cos, tan, ln, abs)"]
        , h2 [][text "Entering Expressions"]
        , text "Enter expressions without any spaces.  You may specify parenthesis to override the normal precedence laws.  Note that there is an issue with the exponentiation operator (^) so if you use it, be explict with parentheses."
        , h2 [][text "Operators"]
        , ul [class "code-block"][
                 li [][text "+ Addition"]
                ,li [][text "- Subtraction"]
                ,li [][text "* Multiplication"]
                ,li [][text "/ Division"]
                ,li [][text "^ Exponentiation"]
            ]
        , p [][text "Note that you must be explicit with operators.  To multiply, you must enter, for example, 3*x not 3x"]
        , h2 [][text "One Variable Functions"]
        , text "If you enter a one-variable function (generally with independent variable lowercase x) you will get a 2-dimensional plot with an x and y axis"
        , text "Here is an example:"
        , div [class "code-block"][text "sin(x)"]
        , text "This will create a 2-dimensional plot (assuming a y axis as the representation of the dependent variable)."
        , img [class "plot-sample-img", src "assets/images/sinx_plot.png "] []
        , h2 [][text "Two Variable Functions"]
        , text "If you enter a two-variable function (generally with independent variables lowercase x and y) you will get a 3-dimensional plot."
        , text "Here is an example:"
        , div [class "code-block"][text "A*sin(W*x)*B*cos(V*y)"]
    ]