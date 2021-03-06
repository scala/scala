object Junk {
  def f(l: List[String]): List[String] = {
    val ret = l.collect({ case MyMatch(id) => id })
    val MyMatch = "(\\d+)".r
    ret
  }

  def test2(): Unit = {
    println(s.length)
    val z = 0
    lazy val s = "abc"
  }

  def test4(): Unit = {
    lazy val x = {
      x
      val z = 0
      lazy val x = 12
      z
    }
  }
}

// updated forwards.scala for lazy vals
object Test {
  lazy val f: Int = x
  val x: Int = f

  {
    lazy val f: Int = x
    val x: Int = f
    println(x)
  }
  {
    lazy val f: Int = g
    var x: Int = f
    lazy val g: Int = x
  }
}
