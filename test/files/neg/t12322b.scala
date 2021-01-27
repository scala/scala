object MyObj {
  val a: Int = 123
  val b: Double = 456.789
  val c: String = "ABC"
}

object ObjectNew extends MyObj.type {
  def main(args: Array[String]): Unit = {
    val mo: MyObj.type = new MyObj.type() // here
  }
}
