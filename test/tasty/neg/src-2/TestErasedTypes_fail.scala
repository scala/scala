package tastytest

object TestErasedTypes {
  import ErasedTypes._

  def test1 = new Bar[Foo]
  def test2 = new Baz[Foo]

}
