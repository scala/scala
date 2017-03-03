 import scala.language.existentials

 class Bar[T]
 class Test {
  def method = {
    object Foo {
      implicit def x: Bar[Foo.type] = new Bar[Foo.type]
    }
    type T = Foo.type

    {
      object Foo
      implicitly[Bar[T]]
    }
  }
}
