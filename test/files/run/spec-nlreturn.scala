
object Test {
  def f(): Int = {
    try {
      val g = (1 to 10 map { i => return 16 ; i }).sum
      g
    }
    catch { case x: runtime.NonLocalReturnControl[_] =>
      println(x.getClass.getName)
      x.value.asInstanceOf[Int]
    }
  }

  def main(args: Array[String]): Unit = {
    println(f())
  }
}
