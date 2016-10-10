object Test {
  var x1 = Array[Object]()
  var x2 = Array[Nothing]()
  var x3 = Array[Null]()

  def f(x: Boolean) = if (x) Array("abc") else Array[Nothing]()
  def g(x: Boolean) = if (x) Array("abc") else Array[Null]()

  def main(args: Array[String]): Unit = {
    println(f(false).length)
    println(f(true).length)
    println(g(false).length)
    println(g(true).length)
    println(x1.length)
    println(x2.length)
    println(x3.length)
  }
}
