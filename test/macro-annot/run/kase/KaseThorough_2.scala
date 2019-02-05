@kase class C[T](x: T, y: Int = 2)(val z: Boolean, val w: String = "")

class KaseThorough {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }
  def assertNotEquals(a: Any, b: Any): Unit = { assert(a != b, s"$a == $b") }
  def assertNotNull(a: Any): Unit = { assert(a != null) }

  val c = C("42")(true)

  def combo = {
    construction
    deconstruction
    copy
    product
    equality
  }

  // @Test
  def construction: Unit = {
    assertEquals(c.toString, "C(42,2)")
    assertEquals(new C("42")(true).toString, "C(42,2)")
    assertEquals(c.x, "42")
    assertEquals(c.y, 2)
    assertEquals(c.z, true)
    assertEquals(c.w, "")
  }

  // @Test
  def deconstruction: Unit = {
    val C(x, y) = c
    assertEquals(x, "42")
    assertEquals(y, 2)
  }

  // @Test
  def copy: Unit = {
    val c1 = c.copy(x = 42)(false, "copied")
    assertEquals(c1.toString, "C(42,2)")
    assertEquals(c1.x, 42)
    assertEquals(c1.y, 2)
    assertEquals(c1.z, false)
    assertEquals(c1.w, "copied")
  }

  // @Test
  def product: Unit = {
    assertEquals((c: Product).productPrefix, "C")
    assertEquals(c.productElement(0), "42")
    assertEquals(c.productElement(1), 2)
    assertEquals(c.productIterator.toList, List("42", 2))
  }

  // @Test
  def equality: Unit = {
    assertEquals(c, c)
    assertNotNull(c)
    assertEquals(c, C("42")(true))
    assertEquals(c, C("42")(false, "something different"))
    assertNotEquals(c, C(43)(true))
  }
}
