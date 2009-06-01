package scala.swing.event

case class WindowDeactivated(override val source: Window) extends WindowEvent(source)
