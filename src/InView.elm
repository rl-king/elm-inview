module InView exposing
    ( State
    , Viewport
    , Element
    , init
    , Msg
    , update
    , updateViewportOffset
    , subscriptions
    , addElements
    , isInView
    , isInOrAboveView
    , Margin
    , isInViewWithMargin
    , isInOrAboveViewWithMargin
    , custom
    )

{-| Detect if an element is visible in the current viewport.


# Definitions

@docs State
@docs Viewport
@docs Element


# Init

@docs init


# Update

@docs Msg
@docs update
@docs updateViewportOffset
@docs subscriptions
@docs addElements


# Check

@docs isInView
@docs isInOrAboveView


# Check with margin

@docs Margin
@docs isInViewWithMargin
@docs isInOrAboveViewWithMargin


# Custom check

@docs custom

-}

import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Process
import Task


{-| Keeps track of viewport position, viewport dimensions and element positions.
-}
type State
    = State
        { elements : Dict String Element
        , viewport : Viewport
        , throttle : Key
        }


type Key
    = Key Int


{-| Similar to Browser.Dom.Viewport with the addition of `maxX` and `maxY`.
-}
type alias Viewport =
    { scene :
        { width : Float
        , height : Float
        }
    , viewport :
        { x : Float
        , y : Float
        , maxX : Float
        , maxY : Float
        , width : Float
        , height : Float
        }
    }


{-| Similar to Browser.Dom.Element without scene and viewport.
-}
type alias Element =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


{-| Takes a list of element ids you want to track and attempts to find them
on the current page.
-}
init : (Msg -> msg) -> List String -> ( State, Cmd msg )
init lift elementIds =
    ( State
        { elements = Dict.empty
        , viewport =
            { scene = { width = 0, height = 0 }
            , viewport = { x = 0, y = 0, maxX = 0, maxY = 0, width = 0, height = 0 }
            }
        , throttle = Key 0
        }
    , Cmd.batch
        [ Task.attempt (lift << GotViewport) Browser.Dom.getViewport
        , addElements lift elementIds
        ]
    )


{-| Track more elements after you've initialized the state.
-}
addElements : (Msg -> msg) -> List String -> Cmd msg
addElements lift elementIds =
    Cmd.batch <|
        List.map (getPosition lift) elementIds


getPosition : (Msg -> msg) -> String -> Cmd msg
getPosition lift id =
    Task.attempt (lift << GotElementPosition id) <|
        Browser.Dom.getElement id



-- SUBSCRIPTIONS


{-| Subscribe to browser resize events to recalculate element positions.
-}
subscriptions : (Msg -> msg) -> State -> Sub msg
subscriptions lift state =
    Browser.Events.onResize (\a b -> lift (OnBrowserResize a b))



-- UPDATE


{-| A message type for the state to update.
-}
type Msg
    = GotViewport (Result () Browser.Dom.Viewport)
    | GotElementPosition String (Result Browser.Dom.Error Browser.Dom.Element)
    | GetElements Key Int Int
    | OnBrowserResize Int Int


{-| Update viewport size and element positions.
-}
update : (Msg -> msg) -> Msg -> State -> ( State, Cmd msg )
update lift msg ((State state) as state_) =
    case msg of
        GotViewport (Ok viewport) ->
            ( State { state | viewport = fromViewport viewport state_ }
            , Cmd.none
            )

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
            let
                nextKey (Key key) =
                    Key (key + 1)
            in
            ( State { state | throttle = nextKey state.throttle }
            , Task.perform (\_ -> lift (GetElements (nextKey state.throttle) width height)) <|
                Process.sleep 500
            )

        GetElements key width height ->
            if state.throttle /= key then
                ( state_, Cmd.none )

            else
                let
                    viewport =
                        state.viewport

                    viewportNested =
                        viewport.viewport
                in
                ( State
                    { state
                        | viewport =
                            { viewport
                                | viewport =
                                    { viewportNested
                                        | width = toFloat width
                                        , height = toFloat height
                                    }
                            }
                    }
                , addElements lift (Dict.keys state.elements)
                )


fromViewport : Browser.Dom.Viewport -> State -> Viewport
fromViewport { viewport, scene } (State state) =
    { scene =
        { width = scene.width
        , height = scene.height
        }
    , viewport =
        { x = viewport.x
        , y = viewport.y
        , maxX = Basics.max viewport.x state.viewport.viewport.maxX
        , maxY = Basics.max viewport.y state.viewport.viewport.maxY
        , width = viewport.width
        , height = viewport.height
        }
    }


{-| Update current viewport `x` and `y` offset.
-}
updateViewportOffset : { x : Float, y : Float } -> State -> State
updateViewportOffset { x, y } (State ({ viewport } as state)) =
    let
        newViewport =
            { scene =
                { width = viewport.scene.width
                , height = viewport.scene.height
                }
            , viewport =
                { x = x
                , y = y
                , maxX = Basics.max x viewport.viewport.maxX
                , maxY = Basics.max y viewport.viewport.maxY
                , width = viewport.viewport.width
                , height = viewport.viewport.height
                }
            }
    in
    State { state | viewport = newViewport }



-- DETECT


{-| True if the element with the given id is in the current viewport.

_note: The result is a Maybe because the element might not be on the page at all._

![check](https://rl-king.github.io/elm-inview-example/illustrations/inView.svg)

-}
isInView : String -> State -> Maybe Bool
isInView id state =
    isInViewWithMargin id noMargin state


{-| True if the element with the given id is in _or_ above the current viewport.

_note: The result is a Maybe because the element might not be on the page at all._

![checkAlt](https://rl-king.github.io/elm-inview-example/illustrations/inViewAlt.svg)

-}
isInOrAboveView : String -> State -> Maybe Bool
isInOrAboveView id state =
    isInOrAboveViewWithMargin id noMargin state



-- PADDING


{-| -}
type alias Margin =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


noMargin : Margin
noMargin =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


{-| True if the element with the given id is in the current viewport but with an offset.
A positive offset will make the viewport smaller and vice versa.

_note: The result is a Maybe because the element might not be on the page at all._

![isInViewWithMargin](https://rl-king.github.io/elm-inview-example/illustrations/inViewWithOffset.svg)

-}
isInViewWithMargin : String -> Margin -> State -> Maybe Bool
isInViewWithMargin id margin state =
    let
        calc { viewport } element =
            (viewport.y + margin.top < element.y + element.height)
                && (viewport.y + viewport.height - margin.bottom > element.y)
                && (viewport.x + margin.left < element.x + element.width)
                && (viewport.x + viewport.width - margin.right > element.x)
    in
    custom (\a b -> Maybe.map (calc a) b) id state


{-| True if the element with the given id is in _or_ above the current viewport but an offset.
A positive offset will make the viewport smaller and vice versa.

_note: The result is a Maybe because the element might not be on the page at all._

![isInOrAboveViewWithMargin](https://rl-king.github.io/elm-inview-example/illustrations/inViewAltWithOffset.svg)

-}
isInOrAboveViewWithMargin : String -> Margin -> State -> Maybe Bool
isInOrAboveViewWithMargin id margin state =
    let
        calc { viewport } element =
            (viewport.y - margin.top + viewport.height > element.y)
                && (viewport.x - margin.left + viewport.width > element.x)
    in
    custom (\a b -> Maybe.map (calc a) b) id state


{-| Write your own check function.

For example `isInOrAboveViewWithMargin` is implemented like:

    isInOrAboveViewWithMargin : String -> Margin -> State -> Maybe Bool
    isInOrAboveViewWithMargin id margin state =
        let
            calc { viewport } element =
                (viewport.y - margin.top + viewport.height > element.y)
                    && (viewport.x - margin.left + viewport.width > element.x)
        in
        custom (\a b -> Maybe.map (calc a) b) id state

_note: Element is a Maybe because the element might not be on the page at all._

-}
custom : (Viewport -> Maybe Element -> a) -> String -> State -> a
custom f id (State { viewport, elements }) =
    f viewport (Dict.get id elements)
