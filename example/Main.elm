port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import InView
import Task



-- PORT


port onScroll : (( Float, Float ) -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map InViewMsg <|
            InView.subscriptions model.inView
        , onScroll OnScroll
        ]



-- MODEL


type alias Model =
    { inView : InView.State
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        ( inViewModel, inViewCmds ) =
            InView.init itemIds
    in
    ( { inView = inViewModel }
    , Cmd.map InViewMsg inViewCmds
    )


itemIds : List String
itemIds =
    List.map String.fromInt <|
        List.range 1 100



-- UPDATE


type Msg
    = OnScroll ( Float, Float )
    | InViewMsg InView.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnScroll ( x, y ) ->
            ( { model | inView = InView.updateViewportOffset x y model.inView }
            , Cmd.none
            )

        InViewMsg inViewMsg ->
            ( { model | inView = InView.update inViewMsg model.inView }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ main_ []
            [ ul [ style "padding" "0" ] <|
                List.map (item model) <|
                    List.range 1 100
            ]
        ]
    }


item : Model -> Int -> Html msg
item model id_ =
    let
        id =
            String.fromInt id_

        className =
            case InView.inViewAltWithOffset id 100 model.inView of
                True ->
                    "slidein"

                _ ->
                    "fadeout"
    in
    li
        [ style "height" "2rem"
        , style "background-color" "gold"
        , style "margin-bottom" "1rem"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "opacity" "0"
        , class className
        , A.id id
        ]
        [ InView.getCenterDistance id model.inView
            |> Maybe.map String.fromFloat
            |> Maybe.withDefault ""
            |> text
        ]
