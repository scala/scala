import Macros._

object Test extends App {
  class D1 extends TM[Int]
  class D2 extends B with TM[Int]

  class D3 extends {} with TM[Int]
  class D4 extends {} with B with TM[Int]

  val x5 = new TM_D[Int]
  val x6 = new TM[Int]{}
  val x7 = new B with TM[Int]{}

  @(TM_A[Int] @TM_A[Int]) class D8

  type T9 = TM[Int] => TM[Int]
  type T10 = (TM[Int], TM[Int])
  type T11 = TM[Int] { def x: Int }
  type T12 = B with TM[Int] { def x: Int }

  new C{} match { case x13: TM[Int] => }

  class D14[T: TM_L[Int]](val x15: TM[Int]) {
    self16: TM[Int] =>
    val x17: TM[Int] = new C{}
    def x18(x19: TM[Int])(x20: TM[Int]): TM[Int] = (x20: TM[Int]): @TM_A[Int]
    type T21 >: TM[Int] <: TM[Int]
    type T22 = TM_L[Int][TM[Int]]
    type T23 = TM_T[Int] forSome { type T }
  }
}