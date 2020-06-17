package tastytest

object TestErasedTypes {
  import ErasedTypes._

  def test1 = new Bar[Foo]
  def test2 = new Baz[Foo]

  def test3(f: Foo) = f.foo1("foo")
  def test4(f: Foo) = f.foo2("foo")

  def test5 = ErasedCompileTimeOps.theNothing

}
