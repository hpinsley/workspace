module Views exposing (view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Iso8601
import Material.Button as Button
import Material.Tab
import Material.TextField as TextField
import Models exposing (..)
import PanelView exposing (viewPanel)
import Time


getFormattedTime : Maybe Time.Posix -> String
getFormattedTime timeInfo =
    case timeInfo of
        Nothing ->
            ""

        Just posixTime ->
            Iso8601.fromTime posixTime


isValidExpression : Model -> Bool
isValidExpression model =
    case model.parsedExpression of
        Nothing ->
            False

        Just _ ->
            True


view : Model -> Html Msg
view model =
    div
        [ id "computations" ]
        [
        -- [ TextField.outlined
        --     (TextField.config
        --         -- |> TextField.setAttributes [ Html.Attributes.id "expression" ]  -- Not working.  Maybe see: https://github.com/aforemny/material-components-web-elm/pull/109
        --         |> TextField.setAttributes [ Html.Attributes.style "width" "100%" ]
        --         |> TextField.setLabel (Just "Expression")
        --         -- |> TextField.setValue model.expression
        --         |> TextField.setValue (Just "inital-value")
        --         |> TextField.setOnInput UpdateExpression
        --     )
        -- 
          label [] [ text "Expression" ]
        , input [ id "expression-input", title "Expression", onInput UpdateExpression, value (Maybe.withDefault "" model.expression) ] []
        , Button.text (Button.config |> Button.setOnClick AddToPanel |> Button.setDisabled (isValidExpression model |> not)) "Add to Panel"
        , div [ id "parseErrors" ] [ text model.parseErrors ]
        , viewPanel model
        , div [ id "time" ]
            [ getFormattedTime model.currentTime |> text
            ]
        ]
