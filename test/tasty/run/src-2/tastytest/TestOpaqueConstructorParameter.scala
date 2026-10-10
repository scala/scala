package tastytest

object TestOpaqueConstructorParameter extends Suite("TestOpaqueConstructorParameter") {
  import OpaqueConstructorParameter._

  test("constructor with opaque type parameters") {
    val holder = new OpaqueHolder(id(42L), id("hello"))

    assert(holder.longId == 42L)
    assert(holder.stringId == "hello")
  }
}
