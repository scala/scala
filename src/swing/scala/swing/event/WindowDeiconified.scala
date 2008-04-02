package scala.swing.event

case class WindowDeiconified(override val source: Frame) extends WindowEvent(source)
