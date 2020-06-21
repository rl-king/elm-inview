# Changelog

## 2.0.0
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

* Remove addElements
* Add addElement

* Throttle refresh on window resize - 300ms
* Throttle addElement - 500ms

## 1.1.0
* Add checkCustom

## 1.0.1
* Fix docs

## 1.0.0
Initial release
