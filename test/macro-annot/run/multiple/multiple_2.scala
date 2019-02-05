
object Test extends App {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  @doubler @doubler case object D

  // @Test def multiple: Unit =
  {
    assertEquals(DDDD.toString, "DDDD")
  }
}
