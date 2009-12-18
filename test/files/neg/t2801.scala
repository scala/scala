object Test {
  def f[A <: AnyRef] = { val a: A = null ; a }
}
