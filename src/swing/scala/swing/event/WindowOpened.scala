package scala.swing.event

case class WindowOpened(override val source: Window) extends WindowEvent(source)
