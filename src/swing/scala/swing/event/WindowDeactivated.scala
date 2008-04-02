package scala.swing.event

case class WindowDeactivated(override val source: Frame) extends WindowEvent(source)
