object Test {
  class C3[T](val elem: T)
  class D3[T](val elemD: T) extends C3[T](elemD)
  def f[T](x: C3[T]) = x match { case d: D3[t] => d.elemD }
}
