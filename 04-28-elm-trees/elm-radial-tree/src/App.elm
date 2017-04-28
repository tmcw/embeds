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
    , generateTreeData 10
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


drawEdge : FlatNode -> Svg.Svg Msg
drawEdge position =
    let
        nodeUp =
            FlatNode (position.depth - 1)
                (floor ((toFloat position.splay) / 2))
                position.value
    in
        path
            [ stroke "#888"
            , strokeWidth "2"
            , d
                ("M"
                    ++ ((List.map
                            (\x -> (List.map toString x |> String.join " "))
                            [ calculateXY nodeUp, calculateXY position ]
                        )
                            |> String.join ", "
                       )
                )
            ]
            []


collectNodePositions : Tree Int -> Int -> Int -> List FlatNode
collectNodePositions tree depth splay =
    case tree of
        Empty ->
            []

        Node v left right ->
            [ FlatNode depth splay v ]
                ++ collectNodePositions left (depth + 1) (splay * 2)
                ++ collectNodePositions right (depth + 1) ((splay * 2) + 1)


ringRadius : Float
ringRadius =
    40.0


percentageToRadian : Float
percentageToRadian =
    pi / 2


calculateXY : FlatNode -> List Float
calculateXY position =
    let
        depth =
            toFloat position.depth

        splay =
            toFloat position.splay

        nodesAtDepth =
            (Basics.max 1 ((2 ^ depth) - 1))

        splayDegree =
            (splay / nodesAtDepth * pi / 2) + (pi / 4)
    in
        List.map (\x -> (x splayDegree) * depth * ringRadius) [ cos, sin ]


translateXY : FlatNode -> Svg.Attribute msg
translateXY position =
    transform
        ("translate("
            ++ (calculateXY position |> (List.map toString) |> String.join ",")
            ++ ")"
        )


drawNode : FlatNode -> Svg.Svg Msg
drawNode position =
    g
        [ translateXY position ]
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


noRoot : FlatNode -> Bool
noRoot position =
    position.depth /= 0


drawTree : Tree Int -> List (Svg.Svg Msg)
drawTree tree =
    (collectNodePositions tree 0 0 |> List.filter noRoot |> List.map drawEdge)
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
                [ Html.text "😂 Radial binary tree printer" ]
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
            [ Html.text """An example of a radial binary tree printer. By
            positioning nodes along the radii of circles, we can ensure
            that the edges never intersect.""" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
