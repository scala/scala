//> using options -Xsource:3
//
object HelloWorld {
  def whileLoop: Int = {
    var i = 0
    var acc = 0
    while (i < 3) {
      var `i'` = 0
      while (`i'` < 4) {
        acc += (i * `i'`)
        `i'` += 1
      }
      i += 1
    }
    acc
  }

  def main(args: Array[String]): Unit = {
    println(s"hello world: ${whileLoop}")
  }
}
