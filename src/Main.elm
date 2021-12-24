module Main exposing (Model, Msg(..), Position, flip, flipGridPos, getGrid, init, main, subscriptions, update, view, viewGrid, viewInput, viewOneGrid)

import Array as A exposing (Array)
import Browser as B
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra.Touch as T
import Svg as S
import Svg.Attributes as SA


main : Program () Model Msg
main =
    B.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { h : Int
    , w : Int
    , isSingleFlipMode : Bool
    , grid : A.Array (A.Array Bool)
    }


type Msg
    = UpdateRange Int Int
    | ChangeMode
    | Flip Position


type alias Position =
    { i : Int
    , j : Int
    }


gridSize : Int
gridSize =
    75


makeFalseGrid : Int -> Int -> Array (A.Array Bool)
makeFalseGrid h w =
    False |> A.repeat w |> A.repeat h


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 3 3 False (makeFalseGrid 3 3)
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRange h w ->
            ( { model | h = h, w = w, grid = makeFalseGrid h w }
            , Cmd.none
            )

        ChangeMode ->
            ( { model | isSingleFlipMode = not model.isSingleFlipMode }
            , Cmd.none
            )

        Flip pos ->
            ( { model | grid = flip model pos }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> H.Html Msg
view model =
    let
        buttonText : String
        buttonText =
            if model.isSingleFlipMode then
                "Normal Mode"

            else
                "Single Flip Mode"
    in
    H.div [ HA.style "height" "100vh" ]
        [ H.div
            [ HA.style "display" "inline-flex"
            , HA.style "width" "100vw"
            , HA.style "height" "10vh"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "space-evenly"
            ]
            [ H.div []
                [ H.text "height:"
                , viewInput "height" (String.fromInt model.h) (\h -> UpdateRange (Basics.max 0 <| Maybe.withDefault 0 <| String.toInt h) model.w)
                ]
            , H.div []
                [ H.text "width:"
                , viewInput "width" (String.fromInt model.w) (\w -> UpdateRange model.h (Basics.max 0 <| Maybe.withDefault 0 <| String.toInt w))
                ]
            , H.button [ HE.onClick ChangeMode ] [ H.text buttonText ]
            ]
        , H.div
            [ HA.style "display" "flex"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "center"
            , HA.style "frex-direction" "column"
            , HA.style "flex-wrap" "wrap"
            , HA.style "height" "100%"
            ]
            [ H.div
                [ HA.style "display" "flex"
                , HA.style "width" "100%"
                , HA.style "align-items" "center"
                , HA.style "justify-content" "space-evenly"
                , HA.style "margin" "0"
                ]
                [ S.svg
                    [ SA.width <| String.fromInt <| gridSize * model.w
                    , SA.height <| String.fromInt <| gridSize * model.h
                    , SA.viewBox <| "0 0 " ++ String.fromInt (gridSize * model.w) ++ " " ++ String.fromInt (gridSize * model.h)
                    ]
                    (viewGrid model)
                ]
            ]
        ]


viewInput : String -> String -> (String -> Msg) -> H.Html Msg
viewInput p v f =
    H.input
        [ HA.type_ "number"
        , HA.placeholder p
        , HA.value v
        , HE.onInput f
        ]
        []


viewGrid : Model -> List (H.Html Msg)
viewGrid model =
    List.range 0 (model.h - 1)
        |> List.map (\i -> List.map (\j -> Position i j) (List.range 0 (model.w - 1)))
        |> List.concat
        |> List.map (\pos -> viewOneGrid model pos)


viewOneGrid : Model -> Position -> H.Html Msg
viewOneGrid model pos =
    S.rect
        [ SA.x <| String.fromInt <| gridSize * pos.j
        , SA.y <| String.fromInt <| gridSize * pos.i
        , SA.width <| String.fromInt gridSize
        , SA.height <| String.fromInt gridSize
        , HA.style "stroke" "gray"
        , HA.style "stroke-width" "3"
        , T.onStart (\_ -> Flip pos)
        , HE.onClick (Flip pos)
        , model.grid
            |> A.get pos.i
            |> Maybe.andThen (A.get pos.j)
            |> Maybe.withDefault False
            |> (\value ->
                    if value then
                        "Black"

                    else
                        "White"
               )
            |> HA.style "fill"
        ]
        []


flip : Model -> Position -> A.Array (A.Array Bool)
flip model pos =
    if model.isSingleFlipMode then
        flipGridPos model.grid pos

    else if not (getGrid model.grid pos) then
        model.grid

    else
        let
            d =
                [ -1, 0, 1 ]
        in
        List.concatMap (\di -> List.map (\dj -> ( di, dj )) d) d
            |> List.foldl
                (\( di, dj ) grid ->
                    let
                        i =
                            pos.i + di

                        j =
                            pos.j + dj
                    in
                    if 0 <= i && i < model.h && 0 <= j && j < model.w then
                        flipGridPos grid (Position i j)

                    else
                        grid
                )
                model.grid


flipGridPos : A.Array (A.Array Bool) -> Position -> A.Array (A.Array Bool)
flipGridPos grid pos =
    let
        value =
            getGrid grid pos

        changed =
            A.get pos.i grid
                |> Maybe.map (A.set pos.j (not value))
                |> Maybe.withDefault A.empty
    in
    A.set pos.i changed grid


getGrid : Array (Array Bool) -> Position -> Bool
getGrid grid pos =
    grid
        |> A.get pos.i
        |> Maybe.andThen (A.get pos.j)
        |> Maybe.withDefault False
