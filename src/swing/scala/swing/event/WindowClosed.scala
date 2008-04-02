package scala.swing.event

case class WindowClosed(override val source: Frame) extends WindowEvent(source)
