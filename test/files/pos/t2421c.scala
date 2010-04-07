object Test {
  class A
  class B
  class C
  class F[X]

  def f(implicit aa: F[A]) = println(aa)

  implicit def a : F[A] = new F[A]()

  // generalised from t2421b to verify we check enough
  class G[X]
  implicit def g[X] = new G[X]()
  implicit def b[X <: B](implicit mx: G[X]) = new F[X]()

  f
}