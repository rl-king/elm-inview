# elm-inview
Detect if an element is in the current viewport

[example live](https://rl-king.github.io/elm-inview-example/) |
[example code](https://github.com/rl-king/elm-inview-example)

![all](https://rl-king.github.io/elm-inview-example/illustrations/All.svg)

Since there is currently no way of listening to scroll events in Elm you'll have to hookup a port. Below is the bit of JS that gets you the scroll position and an example on how to set it all up.

### JS
You might want to throttle or debounce this, listening to scroll events and getting positional information can cause some performance issues.
```js
window.addEventListener("scroll", () => {
    app.ports.onScroll.send([window.scrollX, window.scrollY]);
});
```

### Elm
```elm
port onScroll : (( Float, Float ) -> msg) -> Sub msg


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        ( inViewModel, inViewCmds ) =
            InView.init ["1", "2", "3", "4", "5"]
    in
    ( { inView = inViewModel }
    , Cmd.map InViewMsg inViewCmds
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
       [ Sub.map InViewMsg <|
           InView.subscriptions model.inView
       , onScroll OnScroll
       ]


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
            let
                ( inView, inViewCmds ) =
                    InView.update inViewMsg model.inView
            in
            ( { model | inView = inView }
            , Cmd.map InViewMsg inViewCmds
            )
```
