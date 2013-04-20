object Test {
  val x: Any = null

  def main(args: Array[String]): Unit = {
    println(Option(null.asInstanceOf[Boolean]))
    println(Option(x.asInstanceOf[Boolean]))
    println(Option(null.asInstanceOf[Int]))
    println(Option(x.asInstanceOf[Int]))
    println(Option(null.asInstanceOf[Double]))
    println(Option(x.asInstanceOf[Double]))
    println(Option(null.asInstanceOf[String]))
    println(Option(x.asInstanceOf[String]))
  }
}
