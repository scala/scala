package scala.swing.event

case class WindowDeiconified(override val source: Window) extends WindowEvent(source)
