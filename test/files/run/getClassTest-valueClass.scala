class V(val x: Int) extends AnyVal

object Test {
  def main(args: Array[String]) = {
    val v = new V(2)
    val s: Any = 2
    println(2.getClass)
    println(v.getClass)
  }
}
