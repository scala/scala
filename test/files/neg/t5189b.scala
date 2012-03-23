class TestPos {
  class AbsWrapperCov[+A]
  case class Wrapper[B](x: B) extends AbsWrapperCov[B]

  def unwrap[T](x: AbsWrapperCov[T]): T = x match {
    case Wrapper/*[_ <: T ]*/(x) => x // _ <: T, which is a subtype of T
  }

  def unwrapOption[T](x: Option[T]): T = x match {
    case Some(xs) => xs
  }


  case class Down[+T](x: T)
  case class Up[-T](f: T => Unit)

  def f1[T](x1: Down[T])(x2: Up[T]) = ((x1, x2)) match {
    case (Down(x), Up(f)) => f(x)
  }
}


object TestNeg extends App {
  class AbsWrapperCov[+A]
  case class Wrapper[B](x: Wrapped[B]) extends AbsWrapperCov[B]

  /*
    when inferring Wrapper's type parameter B from x's type AbsWrapperCov[T],
    we must take into account that x's actual type is AbsWrapperCov[Tactual] forSome {type Tactual <: T}
    as AbsWrapperCov is covariant in A -- in other words, we must not assume we know T exactly, all we know is its upper bound

    since method application is the only way to generate this slack between run-time and compile-time types,
    we'll simply replace the skolems that represent method type parameters as seen from the method's body by
    other skolems that are (upper/lower)-bounded by the type-parameter skolems
    (depending on whether the skolem appears in a covariant/contravariant position)
  */
  def unwrap[T](x: AbsWrapperCov[T]): Wrapped[T] = x match {
    case Wrapper/*[_ <: T ]*/(wrapped) => wrapped // : Wrapped[_ <: T], which is a subtype of Wrapped[T] if and only if Wrapped is covariant in its type parameter
  }

  class Wrapped[W](var cell: W) // must be invariant (to trigger the bug)

  // class A { def imNotAB = println("notB")}
  // class B
  //
  // val w = new Wrapped(new A)
  // unwrap[Any](Wrapper(w)).cell = new B
  // w.cell.imNotAB

  def unwrapOption[T](x: Option[T]): T = x match {
    case Some(xs) => xs.foo // the error message should not refer to a skolem (testing extrapolation)
  }

}

// class TestPos1 {
//   class Base[T]
//   case class C[T](x: T) extends Base[T]
//   def foo[T](b: Base[T]): T = b match { case C(x) => x }
//
//   case class Span[K <: Ordered[K]](low: Option[K], high: Option[K]) extends Function1[K, Boolean] {
//     override def equals(x$1: Any): Boolean = x$1 match {
//       case Span((low$0 @ _), (high$0 @ _)) if low$0.equals(low).$amp$amp(high$0.equals(high)) => true
//       case _ => false
//     }
//     def apply(k: K): Boolean = this match {
//       case Span(Some(low), Some(high)) => (k >= low && k <= high)
//       case Span(Some(low), None) => (k >= low)
//       case Span(None, Some(high)) => (k <= high)
//       case _ => false
//     }
//   }
// }
//
// class TestNeg1 {
//   case class Foo[T, U](f: T => U)
//   def f(x: Any): Any => Any = x match { case Foo(bar) => bar }
//   // uh-oh, Any => Any should be Nothing => Any.
// }

