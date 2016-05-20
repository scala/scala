package test

object Tags {
  type Tagged[A, T] = {type Tag = T; type Self = A}

  type @@[T, Tag] = Tagged[T, Tag]

  trait Disjunction

  def meh[M[_], A](ma: M[A]): M[A] = ma
  meh(null.asInstanceOf[Int @@ Disjunction])
}
