package swing.event

case class ContentModified(override val source: Component) extends ComponentEvent(source)
