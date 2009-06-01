package scala.swing.event

case class WindowActivated(override val source: Window) extends WindowEvent(source)
