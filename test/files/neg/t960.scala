sealed abstract class List[+a]
private case object Nil extends List[Nothing]
private final case class Cons[+a](head: a, tail: List[a])
extends List[a]

object List {
  def unapply[a](xs: List[a]): Option[(a, List[a])] = xs match {
    case Nil => None
    case Cons(x, xs) => Some(x, xs)
  }

  def unapply[a](xs: List[a]): Option[Null] = xs match {
    case Nil => Some(null)
    case Cons(_, _) => None
  }

  def foo[a](xs: List[a])  = xs match {
    case List(x, xs) => 7
  }

  def bar(xs: Any)  = xs match { // test error message OverloadedUnapplyError
    case List(x, xs) => 7
  }
}
