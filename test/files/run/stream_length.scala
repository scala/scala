object Test {
  def walk(depth: Int, bias: String): LazyList[String] = {
    if (depth == 0)
      LazyList(bias)
    else {
      (LazyList.iterate(1, 99)(_+1).map((x: Int) => walk(depth-1, bias + x))).flatten
    }
  }

  def main(args: Array[String]): Unit = {
    if (scala.tools.partest.utils.Properties.isAvian) {
      println("!!!TEST SKIPPED!!!")
      println("See scala/bug#7600 for further information.")
    } else
      println("Length: " + walk(3, "---").length)
  }
}
