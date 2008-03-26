package swing.event

case class WindowIconified(override val source: Frame) extends WindowEvent(source)
