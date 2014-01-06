

object Test {
  def walk(depth: Int, bias: String): Stream[String] = {
    if (depth == 0)
      Stream(bias)
    else {
      (Stream.iterate(1, 99)(_+1).map((x: Int) => walk(depth-1, bias + x))).flatten
    }
  }

  def main(args: Array[String]) {
    if (scala.tools.partest.utils.Properties.isAvian) {
      println("!!!TEST SKIPPED!!!")
      println("See SI-7600 for further information.")
    } else
      println("Length: " + walk(3, "---").length)
  }
}
