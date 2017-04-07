object Test extends App {
  def nested: Unit = {
    sealed trait Foo
    object Foo {
      trait Bar extends Foo
      trait Baz extends Foo
    }

    val subs = Macros.knownDirectSubclasses[Foo]
    assert(subs == List("Bar", "Baz"))
  }

  nested
}
