// minimization of failure in run/t4813.scala related to the special
// case for default methods that override methods owned by Object.class in
// Java interfaces.
trait C[A >: Null <: AnyRef] { override def clone(): A = null }
trait X extends C[X]
class D extends X { def foo = (this: X).clone() }
object Test {
  def main(args: Array[String]) {
    assert(new D().foo == null)
  }
}
