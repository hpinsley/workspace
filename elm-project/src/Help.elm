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
        , p [][text "This application allows you to enter a mathematical equation and plot the results.  These common math functions are supported."]
        , ul [][
                 li [][text "sin"]
                ,li [][text "cos"]
                ,li [][text "tan"]
                ,li [][text "ln"]
                ,li [][text "abs"]
                ,li [][text "sqrt"]
            ]
        , h2 [][text "Entering Expressions"]
        , text "Enter expressions without any spaces.  You may specify parenthesis to override the normal precedence laws.  Note that there is an issue with the exponentiation operator (^) so if you use it, be explict with parentheses."
        , h2 [][text "Operators"]
        , ul [][
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
        , text "If you enter a two-variable function (generally with independent variables lowercase x and y) you will get a 3-dimensional plot.  Here is an example."
        , div [class "code-block"][text "A*sin(W*x)*B*cos(V*y)"]
        , img [class "plot-sample-img", src "assets/images/sample-3d-plot.png "] []
        , h2 [][text "Variables and Constants"]
        , text "By convention, variables are lowercase and constants are entered as uppercase.  After parsing the expression, the panel will indicate the variables by showing a check in the Vary column as shown here"
        , img [class "plot-sample-img", src "assets/images/sample-3d-input.png "] []
        , text "The constants are A, B, W, and V and the variables are x and y.  You can change what varies by clicking the Vary column.  Note, however, that at most two things may vary so that a plot can be rendered."
        , h2 [][text "Plotting the Function"]
        , text "Click the Plot button to plot the function.  If it is a 3-d plot (2 variables) you can rotate along the three axes.  The X-Axis is shown in the plot as Red, the Y-Axis as Green, and the Z-Axis as blue."
        , h3 [][text "Rotating the 3-d Plot"]
        , img [class "plot-sample-img", src "assets/images/Axis-Rotation.png "] []
        , text "This section of the panel allows you to rotate along one of the axis.  You can either use the sliders along the axis or you can AUTO ROTATE selecting one of the axis in the AUTO ROTATION SETTINGS section."
    ]