module InView exposing
    ( init
    , subscriptions
    , update
    , updateViewportOffset
    , State
    , Msg
    , inView
    , inViewAlt
    , inViewAltWithOffset
    , inViewWithOffset
    , getCenterDistance
    )

{-|


# Init & Update

@docs init
@docs subscriptions
@docs update
@docs updateViewportOffset


# Definitions

@docs State
@docs Msg


# Detect

@docs inView
@docs inViewAlt
@docs inViewAltWithOffset
@docs inViewWithOffset
@docs getCenterDistance

-}

import Browser.Dom as Dom
import Browser.Events
import Dict exposing (Dict)
import Task



-- DEFINITIONS


type alias Element =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Viewport =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


{-| -}
type State
    = State
        { elements : Dict String Element
        , viewport : Viewport
        }


{-| -}
init : List String -> ( State, Cmd Msg )
init elementIds =
    ( State
        { elements = Dict.empty
        , viewport =
            { x = 0
            , y = 0
            , width = 0
            , height = 0
            }
        }
    , Cmd.batch <|
        Task.attempt GotViewport Dom.getViewport
            :: List.map getPosition elementIds
    )


getPosition : String -> Cmd Msg
getPosition id =
    Task.attempt (GotElementPosition id) <|
        Dom.getElement id



-- SUBSCRIPTIONS


{-| -}
subscriptions : State -> Sub Msg
subscriptions state =
    Browser.Events.onResize OnBrowserResize



-- UPDATE


{-| -}
type Msg
    = GotViewport (Result () Dom.Viewport)
    | GotElementPosition String (Result Dom.Error Dom.Element)
    | OnBrowserResize Int Int


{-| -}
update : Msg -> State -> State
update msg (State ({ viewport } as state)) =
    State <|
        case msg of
            GotViewport (Ok vp) ->
                { state | viewport = vp.viewport }

            GotViewport (Err err) ->
                state

            GotElementPosition id (Ok { element }) ->
                { state | elements = Dict.insert id element state.elements }

            GotElementPosition id (Err err) ->
                state

            OnBrowserResize width height ->
                { state
                    | viewport =
                        { viewport
                            | width = toFloat width
                            , height = toFloat height
                        }
                }


{-| -}
updateViewportOffset : Float -> Float -> State -> State
updateViewportOffset x y (State ({ viewport } as state)) =
    State { state | viewport = { viewport | x = x, y = y } }



-- INVIEW


{-| -}
inView : String -> State -> Bool
inView id state =
    inViewWithOffset id 0 state


{-| -}
inViewWithOffset : String -> Float -> State -> Bool
inViewWithOffset id offset (State { elements, viewport }) =
    case Dict.get id elements of
        Just element ->
            (viewport.y + offset < element.y + element.height)
                && (viewport.y + viewport.height - offset > element.y)
                && (viewport.x + offset < element.x + element.width)
                && (viewport.x + viewport.width - offset > element.x)

        Nothing ->
            False


{-| -}
inViewAlt : String -> State -> Bool
inViewAlt id state =
    inViewAltWithOffset id 0 state


{-| -}
inViewAltWithOffset : String -> Float -> State -> Bool
inViewAltWithOffset id offset (State { elements, viewport }) =
    case Dict.get id elements of
        Just element ->
            (viewport.y - offset + viewport.height > element.y)
                && (viewport.x - offset + viewport.width > element.x)

        Nothing ->
            False


{-| -}
getCenterDistance : String -> State -> Maybe Float
getCenterDistance id (State { elements, viewport }) =
    case Dict.get id elements of
        Just element ->
            Just <|
                (element.y + element.height / 2)
                    - (viewport.y + viewport.height / 2)

        Nothing ->
            Nothing
