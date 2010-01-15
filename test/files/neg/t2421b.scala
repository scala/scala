object Test {
  class A
  class B
  class C
  class F[X]

  def f(implicit aa: F[A]) = println(aa)

  // implicit def a : F[A] = new F[A]()
  implicit def b[X <: B] = new F[X]()

  f
}

/* bug:
error: type arguments [Test2.A] do not conform to method b's type parameter bounds [X <: Test2.B]
*/