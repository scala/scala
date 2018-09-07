object O {
  type A = AnyRef { def x: Int; def a : Int }
  type B = AnyRef { def x: Int; def b : Int }
  type C = AnyRef { def x: Int }

  class U[+X]

  def f(a: U[A], b: U[B]): Seq[U[C]] = Seq(a, b)
  def f_bound_R[X <: B](a: U[A], b: U[X]): Seq[U[C]] = Seq(a, b)
  def f_bound_L[X <: A](a: U[X], b: U[B]): Seq[U[C]] = Seq(a, b)

  type AA = AnyRef { def X: AnyRef { def x: Int; def a : Int } }
  type BB = AnyRef { def X: AnyRef { def x: Int; def b : Int } }
  type CC = AnyRef { def X: AnyRef { def x: Int } }

  def g(a: U[AA], b: U[BB]): Seq[U[CC]] = Seq(a, b)
  def g_bound_R[X <: AA](a: U[AA], b: U[X]): Seq[U[CC]] = Seq(a, b)
  def g_bound_L[X <: BB](a: U[X], b: U[BB]): Seq[U[CC]] = Seq(a, b)

}
