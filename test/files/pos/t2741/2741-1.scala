sealed trait Kleisli[M[_], A, B]

trait PartialApplyKA[T[_[_], _, _], M[_], A] {
  type Apply[B] = T[M, A, B]
}

trait MA[M[_], A]

trait MAs {
  val a: MA[PartialApplyKA[Kleisli, List, String]#Apply, Int] = null
}

object Scalaz extends MAs
