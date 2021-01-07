// scala/bug#2067
object Test1 {
  trait A[B[D[X, Y <: X]]]
  trait C[E[T, S]]
  type T = A[C]
}

// scala/bug#2067
object Test2 {
  trait A[T[_[_]]]
  trait C[X[+_]]
  type T = A[C]
}

// scala/bug#12242
object Test3 {
  trait Box[T]
  trait Adapter[B <: Box[T], T]
  trait Mixin[+A[B <: Box[S], X], S]
  trait Super[T] extends Mixin[Adapter, T]
}
