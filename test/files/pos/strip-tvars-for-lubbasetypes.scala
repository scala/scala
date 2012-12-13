object Test {

  implicit final class EqualOps[T](val x: T) extends AnyVal {
    def ===[T1, Ph >: T <: T1, Ph2 >: Ph <: T1](other: T1): Boolean = x == other
    def !!![T1, Ph2 >: Ph <: T1, Ph >: T <: T1](other: T1): Boolean = x == other
  }

  class A
  class B extends A
  class C extends A

  val a = new A
  val b = new B
  val c = new C

  val x1 = a === b
  val x2 = b === a
  val x3 = b === c // error, infers Object{} for T1
  val x4 = b.===[A, B, B](c)

  val x5 = b !!! c // always compiled due to the order of Ph2 and Ph



}
