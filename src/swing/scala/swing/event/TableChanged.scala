package swing.event

case class TableChanged(table: Table, firstRow: Int, lastRow: Int, column: Int) extends Event {
  println("table changed: "+table+"/"+firstRow+"-"+lastRow+":"+column)
}
