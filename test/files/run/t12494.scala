
trait Base {
  protected[Base] def f: Int
}
object Base {
  class Child extends Base {
    protected[Base] def f: Int = 42
    def test = f
  }
}

object Test extends App {
  assert(new Base.Child().test == 42)
}

/*
was:
t12494.scala:7: error: weaker access privileges in overriding
protected[trait Base] def f: Int (defined in trait Base)
  override should at least be protected[Base]
    protected[Base] def f: Int = 42
                        ^
1 error
*/
