
object Test extends App {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  implicit val x = 42
  @explorer object C

  // @Test def toplevel: Unit =
  {
    assertEquals(A.toString, "can see A, can see B, implicit is <empty>")
    assertEquals(B.toString, "can see A, can see B, implicit is <empty>")
  }

  // @Test def member: Unit =
  {
    assertEquals(C.toString, "can see A, can see B, implicit is <empty>")
  }

  // @Test def local: Unit =
  {
    @explorer object D
    assertEquals(D.toString, "can see A, can see B, implicit is Test.this.x")

    {
      val x = 42
      @explorer object E
      assertEquals(E.toString, "can see A, can see B, implicit is Test.this.x")

      {
        implicit val x = 42
        @explorer object F
        assertEquals(F.toString, "can see A, can see B, implicit is <empty>")
      }
    }
  }
}
