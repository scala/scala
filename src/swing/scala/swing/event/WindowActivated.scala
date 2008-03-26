package swing.event

case class WindowActivated(override val source: Frame) extends WindowEvent(source)
