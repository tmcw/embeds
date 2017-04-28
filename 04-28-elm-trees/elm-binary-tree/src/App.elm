module App exposing (..)

import Html exposing (Html, text, div, img, input)
import Html.Attributes exposing (type_, min, max, value, style)
import Html.Events exposing (onInput)
import String exposing (padLeft)
import Random
import Dict exposing (Dict)
import Svg exposing (svg, circle, g, text_, path)
import Svg.Attributes exposing (width, height, cx, cy, r, transform, fill, textAnchor, d, stroke, strokeWidth, alignmentBaseline, class)


type alias Model =
    { data : List Int
    , tree : Tree Int
    }


type alias ConflictChecker =
    Dict ( Int, Int ) Int


type alias FlatNode =
    { depth : Int
    , splay : Int
    , value : Int
    }


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


init : String -> ( Model, Cmd Msg )
init path =
    ( { data = [], tree = Empty }
    , generateTreeData 5
    )


type Msg
    = NoOp
    | ChangeCount String
    | TreeData (List Int)


empty : Tree a
empty =
    Empty


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
        Empty ->
            Node x Empty Empty

        Node y left right ->
            if x > y then
                Node y left (insert x right)
            else if x < y then
                Node y (insert x left) right
            else
                tree


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


generateTreeData : Int -> Cmd Msg
generateTreeData count =
    Random.generate TreeData
        (Random.list
            count
            (Random.int 0 99)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCount str ->
            ( model, generateTreeData (Result.withDefault 0 (String.toInt str)) )

        TreeData data ->
            ( { model | data = data, tree = fromList data }, Cmd.none )

        _ ->
            ( model, Cmd.none )


drawEdge : Int -> Int -> Int -> Svg.Svg Msg
drawEdge splay depth leftRight =
    path
        [ stroke "#888"
        , strokeWidth "2"
        , d
            ("M"
                ++ (toString (30 * splay))
                ++ ", "
                ++ (toString (30 * depth))
                ++ " "
                ++ (toString (30 * (splay + leftRight)))
                ++ ", "
                ++ (toString (30 * (depth + 1)))
            )
        ]
        []


drawEdges : Tree Int -> Int -> Int -> List (Svg.Svg Msg)
drawEdges tree depth splay =
    case tree of
        Empty ->
            []

        Node v left right ->
            List.concat
                [ case left of
                    Node v _ _ ->
                        [ drawEdge splay depth -1 ]

                    Empty ->
                        []
                , case right of
                    Node v _ _ ->
                        [ drawEdge splay depth 1 ]

                    Empty ->
                        []
                , drawEdges left (depth + 1) (splay - 1)
                , drawEdges right (depth + 1) (splay + 1)
                ]


flatNodeToTuple : FlatNode -> ConflictChecker -> ConflictChecker
flatNodeToTuple node dict =
    Dict.update ( node.depth, node.splay )
        (\v -> Just ((Maybe.withDefault 0 v) + 1))
        dict


keepDuplicates : ( Int, Int ) -> Int -> Bool
keepDuplicates k v =
    v > 1


getConflicts : Tree Int -> List ( Int, Int )
getConflicts tree =
    collectNodePositions tree 0 0
        |> List.foldr flatNodeToTuple Dict.empty
        |> Dict.filter keepDuplicates
        |> Dict.keys


collectNodePositions : Tree Int -> Int -> Int -> List FlatNode
collectNodePositions tree depth splay =
    case tree of
        Empty ->
            []

        Node v left right ->
            [ FlatNode splay depth v ]
                ++ collectNodePositions left (depth + 1) (splay - 1)
                ++ collectNodePositions right (depth + 1) (splay + 1)


translateXY : Int -> Int -> Svg.Attribute msg
translateXY x y =
    transform
        ("translate("
            ++ (toString (30 * x))
            ++ ", "
            ++ (toString (30 * y))
            ++ ")"
        )


drawConflictNode : ( Int, Int ) -> Svg.Svg Msg
drawConflictNode position =
    g
        [ translateXY (Tuple.first position) (Tuple.second position) ]
        [ circle
            [ r "20"
            , fill "#f08"
            , class "pulse"
            ]
            []
        ]


drawNode : FlatNode -> Svg.Svg Msg
drawNode position =
    g
        [ translateXY position.depth position.splay ]
        [ circle
            [ r "15"
            , strokeWidth "2"
            , fill "#fff"
            , stroke "#000"
            ]
            []
        , g
            [ transform "translate(0, 2)" ]
            [ text_
                [ textAnchor "middle"
                , alignmentBaseline "middle"
                ]
                [ text (toString position.value) ]
            ]
        ]


drawTree : Tree Int -> List (Svg.Svg Msg)
drawTree tree =
    drawEdges tree 0 0
        ++ (getConflicts tree |> List.map drawConflictNode)
        ++ (collectNodePositions tree 0 0 |> List.map drawNode)


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "width"
              , "640px"
              )
            ]
        ]
        [ div [ style [ ( "display", "flex" ), ( "padding", "0 0 10px 0" ), ( "border-bottom", "1px solid #222" ) ] ]
            [ div []
                [ Html.text "😕 Bad binary tree printer" ]
            , div
                [ style [ ( "flex-grow", "1" ), ( "text-align", "center" ) ] ]
                [ input
                    [ type_ "range"
                    , Html.Attributes.class "slider"
                    , Html.Attributes.min "1"
                    , Html.Attributes.max "40"
                    , onInput ChangeCount
                    , value (List.length model.data |> toString)
                    ]
                    []
                ]
            , div
                [ style [ ( "white-space", "pre" ) ]
                ]
                [ Html.text ("Count: " ++ (List.length model.data |> toString |> padLeft 4 ' ')) ]
            ]
        , svg
            [ width "640"
            , height "400"
            ]
            [ g
                [ transform "translate(320, 50)"
                ]
                (drawTree model.tree)
            ]
        , div []
            [ Html.text """An example of a naïve binary tree printer.
            It draws the tree from the top down, moving down and to the left
            and to the right to draw children. Move the slider to choose different
            tree sizes, and see the magenta-highlighted nodes that incorrectly
            overlap with other nodes from different branches of the tree.""" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
