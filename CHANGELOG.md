# Changelog

## 2.0.1
* Bump to create new tag in an attempt to fix #5

## 2.0.0
* Pass `Msg -> msg` function to every that returns a `Msg`
* Add Viewport
  * Like Browser.Dom.Viewport with max `x` and `y`
* Add Element
  * Like Browser.Dom.Element without `scene` and `viewport`

* Rename checkCustom -> custom
  * Returns `a` instead of `Maybe Bool`
* Rename check -> isInView
* Rename checkAlt -> isInOrAboveView
* Rename checkWithOffset -> isInViewWithMargin
* Rename checkAltWithOffset -> isInOrAboveViewWithMargin

* Add Margin
  * Allows for control over all edges instead of just horizonal and vertical

* Refresh elements on window resize events - 300ms of no change
* Batch DOM lookup when adding more elements - 500ms of no change

## 1.1.0
* Add checkCustom

## 1.0.1
* Fix docs

## 1.0.0
Initial release
