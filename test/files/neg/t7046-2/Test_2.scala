object Test extends App {
  def nested: Unit = {
    val subs = Macros.knownDirectSubclasses[Foo]
    assert(subs == List("Bar", "Baz"))

    sealed trait Foo
    object Foo {
      trait Bar extends Foo
      trait Baz extends Foo
    }
  }

  nested
}
