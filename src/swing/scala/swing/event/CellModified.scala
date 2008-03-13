package swing.event

case class CellModified(col: Int, row: int, value: String) extends Event
