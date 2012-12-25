import Macros._

object Test extends App {
  class D1 extends TM
  class D2 extends B with TM

  class D3 extends {} with TM
  class D4 extends {} with B with TM

  val x5 = new TM_D
  val x6 = new TM{}
  val x7 = new B with TM{}

  @(TM_A @TM_A) class D8

  type T9 = TM => TM
  type T10 = (TM, TM)
  type T11 = TM { def x: Int }
  type T12 = B with TM { def x: Int }

  new C{} match { case x13: TM => }

  // class D14[T: TM_L](val x15: TM) {
  class D14(val x15: TM) {
    self16: TM =>
    val x17: TM = new C{}
    def x18(x19: TM)(x20: TM): TM = (x20: TM): @TM_A
    type T21 >: TM <: TM
    // type T22 = TM_L[TM]
    type T23 = TM_T forSome { type T }
  }
}