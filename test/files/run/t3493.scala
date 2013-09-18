



object Test {

  def main(args: Array[String]) {
    import scala.collection.immutable._
    val x = TreeSet("a", "b", "c", "d")
    val x2 = x + "e"
    assert(x2.toString == "TreeSet(a, b, c, d, e)")
    assert(x2.toString == runtime.ScalaRunTime.stringOf(x2).trim)
  }

}
