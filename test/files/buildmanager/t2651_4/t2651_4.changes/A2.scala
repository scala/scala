trait A[S, T] {
  def x: T
  def y(a: T)
  def z[B <: T]
}
