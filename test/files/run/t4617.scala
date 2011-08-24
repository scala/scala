object Test {
  def f1 = new { def f { lazy val d = 0d } }
  def f2 = {
    lazy val d = 4D
    lazy val f = 4f

    def bar = "Str " + (d + f)
    bar
  }

  def main(args: Array[String]): Unit = {
    f1
    println(f2)
  }
}
