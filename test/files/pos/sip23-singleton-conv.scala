object Test {
  sealed trait Nat
  sealed trait Succ[N <: Nat] extends Nat
  sealed trait _0 extends Nat
  object _0 extends _0

  object Nat {
    implicit def zero(i: 0): _0 = _0
  }

  trait Unroll[-N <: Nat] {
    type Out
  }

  object Unroll {
    implicit def zero: Unroll[_0] { type Out = Int } = ???
  }

  def unroll(n: Nat)(implicit u: Unroll[n.type]): u.Out = ???
  val u0 = unroll(0)
  u0: Int
}
