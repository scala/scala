trait NewType[X]

object Test {
  // change return type to Foo and it compiles.
  implicit def Unwrap[X](n: NewType[X]): X = sys.error("")
  class Foo(val a: Int)
  def test(f: NewType[Foo]) = f.a
}


