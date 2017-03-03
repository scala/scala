trait C[-I] {
  private[this] type X = C[I]
  def f2(b: X): Unit
}
