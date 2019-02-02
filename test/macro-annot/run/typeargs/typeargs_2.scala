
object Test extends App {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  // @Test def macroAnnotationsWithTypeArgsExpand: Unit =
  {
    @shove[Int] val description = "I’m an Int!"
    @shove[String] val bar = "I’m a String!"
    assertEquals(5.description, "I’m an Int!")
    assertEquals("foo".bar, "I’m a String!")
  }
}
