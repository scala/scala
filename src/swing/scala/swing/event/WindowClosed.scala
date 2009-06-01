package scala.swing.event

case class WindowClosed(override val source: Window) extends WindowEvent(source)
