case class Check[A](val value: A)

case class C2(checks: Check[_]*);

object C {
  def m(x : C2): Any = (null: Any) match {
    case C2(_, rest @ _*) => {
      rest.map(_.value)
    }
  }
}

///////////////////

object Container {
  trait Exp[+T]
  abstract class FuncExp[-S, +T]

  sealed abstract class FoundNode[T, Repr] {
    def optimize[TupleT, U, That](parentNode: FlatMap[T, Repr, U, That]): Any
    def optimize2[TupleT, U, That](parentNode: Any): Any
  }

  class FlatMap[T, Repr, U, That]

  val Seq(fn: FoundNode[t, repr]) = Seq[FoundNode[_, _]]()
  fn.optimize(null)    // was: scala.MatchError: ? (of class BoundedWildcardType) @ Variances#varianceInType
  fn.optimize2(null) // was: fatal error: bad type: ?(class scala.reflect.internal.Types$BoundedWildcardType) @ Pickle.putType
}
