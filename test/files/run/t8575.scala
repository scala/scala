class E[F]
class A
class B
class C

trait TypeMember {
  type X

  // This call throws an AbstractMethodError, because it invokes the erasure of
  // consume(X): Unit that is consume(Object): Unit. But the corresponding
  // bridge method is not generated.
  consume(value)

  def value: X
  def consume(x: X): Unit
}

object Test extends App with TypeMember {
  type F = A with B

  // works if replaced by type X = E[A with B with C]
  type X = E[F with C]

  val value = new E[F with C]

  // This call passes, since it invokes consume(E): Unit
  consume(value)
  def consume(x: X) {}
}