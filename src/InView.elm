module InView exposing
    ( init
    , subscriptions
    , update
    , updateViewportOffset
    , addElements
    , State
    , Msg
    , inView
    , inViewWithOffset
    , inViewAlt
    , inViewAltWithOffset
    , CenterDistance
    , centerDistance
    )

{-|


# Init & Update

@docs init
@docs subscriptions
@docs update
@docs updateViewportOffset
@docs addElements


# Definitions

@docs State
@docs Msg


# Detect

@docs inView
@docs inViewWithOffset
@docs inViewAlt
@docs inViewAltWithOffset


# CenterDistance

@docs CenterDistance
@docs centerDistance

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


{-| Keeps track of viewport position, viewport dimensions and element positions
-}
type State
    = State
        { elements : Dict String Element
        , viewport : Viewport
        }


{-| Takes the list of element ids you want to keep track of
-}
init : List String -> ( State, Cmd Msg )
init elementIds =
    ( State
        { elements = Dict.empty
        , viewport = Viewport 0 0 0 0
        }
    , Cmd.batch
        [ Task.attempt GotViewport Dom.getViewport
        , addElements elementIds
        ]
    )


{-| Add elements you'd like to be able to detect
-}
addElements : List String -> Cmd Msg
addElements elementIds =
    Cmd.batch <|
        List.map getPosition elementIds


getPosition : String -> Cmd Msg
getPosition id =
    Task.attempt (GotElementPosition id) <|
        Dom.getElement id



-- SUBSCRIPTIONS


{-| Subscribes to browser resize events and recalculates element positions
-}
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
update : Msg -> State -> ( State, Cmd Msg )
update msg (State ({ viewport } as state)) =
    case msg of
        GotViewport (Ok vp) ->
            ( State { state | viewport = vp.viewport }, Cmd.none )

        GotViewport (Err err) ->
            ( State state, Cmd.none )

        GotElementPosition id (Ok { element }) ->
            ( State
                { state
                    | elements =
                        Dict.insert id element state.elements
                }
            , Cmd.none
            )

        GotElementPosition id (Err err) ->
            ( State { state | elements = Dict.remove id state.elements }
            , Cmd.none
            )

        OnBrowserResize width height ->
            ( State
                { state
                    | viewport =
                        { viewport
                            | width = toFloat width
                            , height = toFloat height
                        }
                }
            , addElements (Dict.keys state.elements)
            )


{-| Update current viewport offset
-}
updateViewportOffset : Float -> Float -> State -> State
updateViewportOffset x y (State ({ viewport } as state)) =
    State { state | viewport = { viewport | x = x, y = y } }



-- DETECT


{-| True if the element is in the current viewport

![inView](https://rl-king.github.io/elm-inview-example/illustrations/inView.svg)

-}
inView : String -> State -> Maybe Bool
inView id state =
    inViewWithOffset id 0 0 state


{-| True if the element is in the current viewport but with an x and y offset.
A positive offset will make the viewport smaller and vice versa.

![inViewWithOffset](https://rl-king.github.io/elm-inview-example/illustrations/inViewWithOffset.svg)

-}
inViewWithOffset : String -> Float -> Float -> State -> Maybe Bool
inViewWithOffset id offsetX offsetY (State { elements, viewport }) =
    let
        calc element =
            (viewport.y + offsetY < element.y + element.height)
                && (viewport.y + viewport.height - offsetY > element.y)
                && (viewport.x + offsetX < element.x + element.width)
                && (viewport.x + viewport.width - offsetX > element.x)
    in
    Maybe.map calc (Dict.get id elements)


{-| True if the element is in _or_ above the current viewport

![inViewAlt](https://rl-king.github.io/elm-inview-example/illustrations/inViewAlt.svg)

-}
inViewAlt : String -> State -> Maybe Bool
inViewAlt id state =
    inViewAltWithOffset id 0 0 state


{-| True if the element is in _or_ above the current viewport but with an x and y offset.
A positive offset will make the viewport smaller and vice versa.

![inViewAltWithOffset](https://rl-king.github.io/elm-inview-example/illustrations/inViewAltWithOffset.svg)

-}
inViewAltWithOffset : String -> Float -> Float -> State -> Maybe Bool
inViewAltWithOffset id offsetX offsetY (State { elements, viewport }) =
    let
        calc element =
            (viewport.y - offsetY + viewport.height > element.y)
                && (viewport.x - offsetX + viewport.width > element.x)
    in
    Maybe.map calc (Dict.get id elements)



-- DISTANCE


{-| The distance of the element's center to viewport center in px
-}
type alias CenterDistance =
    { x : Float
    , y : Float
    }


{-| CenterDistance from viewport center.

Useful for creating positional effects relative to the viewport

-}
centerDistance : String -> State -> Maybe CenterDistance
centerDistance id (State { elements, viewport }) =
    let
        calc element =
            { x =
                (element.x + element.width / 2)
                    - (viewport.x + viewport.width / 2)
            , y =
                (element.y + element.height / 2)
                    - (viewport.y + viewport.height / 2)
            }
    in
    Maybe.map calc (Dict.get id elements)
