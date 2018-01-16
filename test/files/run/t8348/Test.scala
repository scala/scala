object Test extends App {
  import j._

  val column = new TableColumnImpl(1)

  println(column.width)
  println((column: TableColumn).width)

}
