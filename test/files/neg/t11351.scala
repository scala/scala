class X
class Y extends X

trait A[+T] {
  def foo: T = null.asInstanceOf[T]
}

trait B extends A[X] {
  override def foo: X = new X
}

trait C extends A[Y] {
  override def foo: Y = new Y
  def superFoo: Y = super.foo // C will have an abstract `def C$$super$foo: Y` because of this call
}

class Fail extends B with C
// Should generate `def C$$super$foo: Y = super[A].foo` and not `= super[B].foo`

object Test {
  def main(args: Array[String]): Unit = {
    val y: Y = (new Fail).superFoo // Used to fail with a ClassCastException because of `Fail#C$$super$foo` being incorrect above
    assert(y == null)
  }
}
