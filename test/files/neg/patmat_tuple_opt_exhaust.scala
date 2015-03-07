sealed abstract class L[A]
final case class N[A]() extends L[A]
final case class C[A](head: A, tail: L[A]) extends L[A]

object Test {
  def exhaust[A](a1: L[A], a2: L[A]): Unit =
    (a1, a2) match {
      // case (N(), N())         =>
      // case (N(), C(_, _))     =>
      // case (C(_, _), N())     =>
      // case (C(_, _), C(_, _)) =>
      case (null, null)          =>
    }
}
