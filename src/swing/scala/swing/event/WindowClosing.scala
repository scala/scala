package swing.event

case class WindowClosing(override val source: Frame) extends WindowEvent(source)
