object Test {
  val label = "name"
  val table: Table = sys.error("")
  table.addColumn( label, label.getClass )
}
