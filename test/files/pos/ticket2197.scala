trait PartialType[T[_, _], A] {
  type Apply[B] = T[A, B]
}

sealed trait State[S, +A]
trait Pure[P[_]]
trait StatePure[X] extends Pure[PartialType[State, X]#Apply]
