package scala.swing.event

case class WindowIconified(override val source: Window) extends WindowEvent(source)
