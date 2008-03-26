package swing.event

case class TableResized(override val source: Table) extends ComponentEvent(source)
