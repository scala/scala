class T {
  def f[C[X] <: C[X]](l: C[_]) = l.x
}
