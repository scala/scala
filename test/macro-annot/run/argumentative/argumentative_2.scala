
object Test extends App {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  // @Test def combo: Unit =
  {
    @argumentative(1, 2) object X
    assertEquals(X.toString, "1 2")
  }
}
