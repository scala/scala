object Test {
  trait Low[X]
  trait Lower[V, X] extends Low[X]
  trait Res[V]
  trait High[L[X] <: Low[X]]
  trait HighRes[L[X] <: Lower[V, X], V] extends Res[V]
  trait Mid[X, Y]

  def m[L[X] <: Lower[V, X], V](high: High[L]): HighRes[L, V] = ???
  def m[X, Y](mid: Mid[X, Y]): Res[Y] = ???
  def ok[L[X] <: Lower[V, X], V](high :High[L]): HighRes[L, V] = m(high)
  def wtf[L[X] <: Lower[V, X], V](high :High[L]): HighRes[L, V] = m[L, V](high)
}