class A {
  def f[CC[X] <: Traversable[X]](x: CC[Int]) = ()

  f(1 to 5)
}
