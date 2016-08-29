trait C[-I] {
  protected[this] type X = C[I]
  def f2(b: X): Unit
}
