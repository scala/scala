package swing.event

case class WindowOpened(override val source: Frame) extends WindowEvent(source)
