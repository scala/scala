package scala.swing.event

import geometry._

class MouseEvent(source: Component, point: Point, modifiers: Int) extends ComponentEvent(source)

class MouseButtonEvent(source: Component, point: Point, modifiers: Int,
                     clicks: Int, triggersPopup: Boolean)
      extends MouseEvent(source, point, modifiers)
case class MouseClicked(override val source: Component, point: Point, modifiers: Int,
                     clicks: Int, triggersPopup: Boolean)
           extends MouseButtonEvent(source, point, modifiers, clicks, triggersPopup)
case class MousePressed(override val source: Component, point: Point, modifiers: Int,
                        clicks: Int, triggersPopup: Boolean)
           extends MouseButtonEvent(source, point, modifiers, clicks, triggersPopup)
case class MouseReleased(override val source: Component, point: Point, modifiers: Int,
                        clicks: Int, triggersPopup: Boolean)
           extends MouseButtonEvent(source, point, modifiers, clicks, triggersPopup)

class MouseMotionEvent(override val source: Component, point: Point, modifiers: Int)
      extends MouseEvent(source, point, modifiers)
case class MouseMoved(override val source: Component, point: Point, modifiers: Int)
           extends MouseMotionEvent(source, point, modifiers)
case class MouseDragged(override val source: Component, point: Point, modifiers: Int)
           extends MouseMotionEvent(source, point, modifiers)
case class MouseEntered(override val source: Component, point: Point, modifiers: Int)
           extends MouseMotionEvent(source, point, modifiers)
case class MouseExited(override val source: Component, point: Point, modifiers: Int)
           extends MouseMotionEvent(source, point, modifiers)

case class MouseWheelMoved(override val source: Component, point: Point, modifiers: Int, rotation: Int)
           extends MouseEvent(source, point, modifiers)