package scala.swing.event

import java.awt.Point

class MouseEvent(val source: Component, point: Point, val modifiers: Int)(val when: Long) extends InputEvent

class MouseButtonEvent(source: Component, point: Point, override val modifiers: Int,
                     clicks: Int, triggersPopup: Boolean)(when: Long)
      extends MouseEvent(source, point, modifiers)(when)
case class MouseClicked(override val source: Component, point: Point, override val modifiers: Int,
                     clicks: Int, triggersPopup: Boolean)(when: Long)
           extends MouseButtonEvent(source, point, modifiers, clicks, triggersPopup)(when)
case class MousePressed(override val source: Component, point: Point, override val modifiers: Int,
                        clicks: Int, triggersPopup: Boolean)(when: Long)
           extends MouseButtonEvent(source, point, modifiers, clicks, triggersPopup)(when)
case class MouseReleased(override val source: Component, point: Point, override val modifiers: Int,
                        clicks: Int, triggersPopup: Boolean)(when: Long)
           extends MouseButtonEvent(source, point, modifiers, clicks, triggersPopup)(when)

class MouseMotionEvent(source: Component, point: Point, modifiers: Int)(when: Long)
      extends MouseEvent(source, point, modifiers)(when)
case class MouseMoved(override val source: Component, point: Point, override val modifiers: Int)(when: Long)
           extends MouseMotionEvent(source, point, modifiers)(when)
case class MouseDragged(override val source: Component, point: Point, override val modifiers: Int)(when: Long)
           extends MouseMotionEvent(source, point, modifiers)(when)
case class MouseEntered(override val source: Component, point: Point, override val modifiers: Int)(when: Long)
           extends MouseMotionEvent(source, point, modifiers)(when)
case class MouseExited(override val source: Component, point: Point, override val modifiers: Int)(when: Long)
           extends MouseMotionEvent(source, point, modifiers)(when)

case class MouseWheelMoved(override val source: Component, point: Point, override val modifiers: Int, rotation: Int)(when: Long)
           extends MouseEvent(source, point, modifiers)(when)