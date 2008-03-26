package swing.event

case class TableChanged(override val source: Table, firstRow: Int, lastRow: Int, column: Int) extends ComponentEvent(source) {
  println("table changed: "+source+"/"+firstRow+"-"+lastRow+":"+column)
}
