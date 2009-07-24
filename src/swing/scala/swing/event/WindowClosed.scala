package scala.swing
package event

case class WindowClosed(override val source: Window) extends WindowEvent(source)
