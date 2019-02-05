case class B(var x: Int) {
  def succ(): Unit = {
    x = x + 1
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val b = B(0)
    b match {
      case B(x) =>
        //println(x)
        b.succ()
        println(x)
    }
  }
}
