class A {
  @native def setup(): Unit

  // also kosher
  @native private def f(): Unit
  @native final def g(): Unit
}
