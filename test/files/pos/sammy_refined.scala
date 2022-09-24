trait DepFn[-A] {
  type Out
  def apply(in: A): Out
}

object DepFn {
  type Aux[-A, B] = DepFn[A] { type Out = B }
  type AuxF[F[_], A] = Aux[F[A], F[A]] { type B >: A }
  val length: DepFn[String] { type Out = Int } = _.length
  val upper: Aux[String, String] = _.toUpperCase
  val reverse: AuxF[List, Int] = _.reverse
}

class Outer {
  // T here does not compile to a SAM in bytecode,
  // because of the outer reference to the enclosing class.
  trait T { def f(x: Int): Int }
  val t1: T = x => x
  val t2: T { type U = String } = x => x
  val t3: T { type U } = x => x
}
