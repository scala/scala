object Test {
  val xs = List(Array[String](), Array[Nothing]())
  var ys = xs.head

  def main(args: Array[String]): Unit = {
    ys = xs.last
  }
}
