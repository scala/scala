package scala.swing.event

case class WindowClosing(override val source: Window) extends WindowEvent(source)
