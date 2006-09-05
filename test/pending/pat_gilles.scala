abstract class Table2 {


  val x: Any => Unit = { zz:Any =>
    zz match {
    case Table2.CellUpdated(row, column) =>
      val foo = Table2.CellUpdated(2,2)
      Console.println("cuckoo")
  }}

}

object Table2 {

  case class CellUpdated(row: Int, column: Int)

}
