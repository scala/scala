package swing.event

case class CaretUpdate(override val source: TextComponent) extends ComponentEvent(source)
