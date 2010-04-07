trait A[T, S] {
  def x: T
  def y(a: T)
  def z[B <: T]
}
