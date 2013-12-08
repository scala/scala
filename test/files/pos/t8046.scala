trait One {
  type Op[A]
  type Alias[A] = Op[A]
}
 
trait Two extends One {
  trait Op[A] extends (A => A)
 
  // This compiles
  class View1 extends Op[Int] { def apply(xs: Int) = xs }
 
  // ??? base class View2 not found in basetypes of class View2
  // ./a.scala:9: error: class View2 needs to be abstract, since \
  //   method apply in trait Function1 of type (v1: T1)R is not defined
  // (Note that T1 does not match Int)
  //   class View2 extends Alias[Int] { def apply(xs: Int) = xs }
  //         ^
  // one error found
  class View2 extends Alias[Int] { def apply(xs: Int) = xs }
}
