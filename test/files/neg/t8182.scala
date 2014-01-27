
trait X {
  val a b     // something missing
}
trait Y {
  val a b[B]  // error then continue as for X
}
trait Z {
  (null: Any) match {
    case a b[B] => // bumpy recovery
  }
}
object B { def unapply[W](a: Any) = Some((1,2)) }
trait Z {
  (null: Any) match {
    case a B[T] b =>
  }
}
