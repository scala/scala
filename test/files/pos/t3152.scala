trait Applicative[M[_]]

sealed trait MA[M[_], A] {
  def sequence[N[_], B](implicit a: A <:< N[B], n: Applicative[N]): N[M[B]] = sys.error("stub")
  // def sequence3[N[_], B]()(implicit a: A <:< N[B], n: Applicative[N]): N[M[B]] = sys.error("stub")
}

object test {
  implicit def ListMA[A](l: List[A]): MA[List, A] = sys.error("stub")
  implicit val ao: Applicative[Option] = sys.error("stub")

  /* This compiles OK:
  (Nil: List[Option[Int]]).sequence3(): Option[List[Int]]
  */

  // BUG: error: immutable is not an enclosing class
  // !!! No line number is reported with the error
  (Nil: List[Option[Int]]).sequence: Option[List[Int]]
  (List[Option[Int]]()).sequence: Option[List[Int]]
}
