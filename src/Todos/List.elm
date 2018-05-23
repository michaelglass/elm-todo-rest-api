module Todos.List exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Todos.Messages exposing (Msg(ChangePriorityInline, Complete, Delete, DeleteCompleted, Patch, Revert, ShowEditView))
import Todos.Models exposing (Todo, TodoEditView(Editing))


-- this module contains the todo list view
-- it explains itself for the most part, it is just html
-- just some common styles for table cells (th and td)


cellStyle : List ( String, String )
cellStyle =
    [ ( "textAlign", "left" )
    , ( "padding", "10px" )
    ]



-- make a cell (th or td) with the pre-set style attribute


cell : (List (Attribute msg) -> a -> b) -> a -> b
cell el children =
    el [ style cellStyle ] children



-- the main view here is a table with headers and body rows for each todo


sortByPriority : List Todo -> List Todo
sortByPriority =
    List.sortBy .priority


view : List Todo -> Html Msg
view todos =
    div []
        [ table []
            [ thead []
                [ cell th [ text "Id" ]
                , cell th [ text "Title" ]
                , cell th [ text "Priority" ]
                , cell th [ text "Completed?" ]
                , cell th [ text "Actions" ]
                ]

            -- below, we keep things modular by mapping a todo row view to every todo
            , todos
                |> sortByPriority
                |> List.map todo
                |> tbody []

            -- note:
            -- instead, the above could have been:
            --     tbody [] (List.map todo todos)
            -- but, it does demonstrate a good use of the
            -- right-to-left function application operator
            ]
        , footer
        ]



-- a single todo row


todo : Todo -> Html Msg
todo t =
    let
        -- destructure our todo
        { id, title, priority, completed } =
            t

        -- decide on some UI text/actions based on todo completed status
        ( completedText, buttonText, buttonMsg ) =
            if completed then
                ( "Yes", "Revert", Revert )
            else
                ( "No", "Done", Complete )
    in
    tr []
        [ cell td [ text <| toString id ]
        , cell td [ text title ]
        , cell td [ input [ onInput (ChangePriorityInline t), value (toString priority) ] [] ]
        , cell td [ text completedText ]
        , cell td
            [ button
                [ onClick <| buttonMsg t ]
                [ text buttonText ]
            , button
                [ onClick <| ShowEditView <| Editing t ]
                [ text "Edit" ]
            , button
                [ onClick <| Delete t ]
                [ text "Delete" ]
            ]
        ]



-- footer


footer : Html Msg
footer =
    div []
        [ br [] []
        , button
            [ onClick DeleteCompleted ]
            [ text "Clear Completed" ]
        ]
