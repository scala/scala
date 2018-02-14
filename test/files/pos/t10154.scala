trait Bar2[T]

object Test2 {
  def wrap: Unit = {
    object Foo {
      implicit def fooBar: Bar2[Foo.type] = ???
    }

    implicitly[Bar2[Foo.type]]
  }
}
