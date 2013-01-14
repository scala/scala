import Macros._

object Test extends App {
  class D1 extends TM(2)(3)
  class D2 extends B with TM(2)(3)

  class D3 extends {} with TM(2)(3)
  class D4 extends {} with B with TM(2)(3)

  val x5 = new TM_D(2)(3)
  val x6 = new TM(2)(3){}
  val x7 = new B with TM(2)(3){}

  @(TM_A(2)(3) @TM_A(2)(3)) class D8

  type T9 = TM(2)(3) => TM(2)(3)
  type T10 = (TM(2)(3), TM(2)(3))
  type T11 = TM(2)(3) { def x: Int }
  type T12 = B with TM(2)(3) { def x: Int }

  new C{} match { case x13: TM(2)(3) => }

  class D14[T: TM_L(2)(3)](val x15: TM(2)(3)) {
    self16: TM(2)(3) =>
    val x17: TM(2)(3) = new C{}
    def x18(x19: TM(2)(3))(x20: TM(2)(3)): TM(2)(3) = (x20: TM(2)(3)): @TM_A(2)(3)
    type T21 >: TM(2)(3) <: TM(2)(3)
    type T22 = TM_L(2)(3)[TM(2)(3)]
    type T23 = TM_T(2)(3) forSome { type T }
  }
}