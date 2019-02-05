case class A[X](val a: X) extends AnyVal
case class B[X <: Serializable](val b: X) extends AnyVal

object Test extends App {
  val it = Array(A(1), A("foo"))
  it(0) = A(123)
  it.head
  it.last

  val that = Array(A("baz"), A('fff))
  that.head
  that.last
}
