object Test {
  class A
  class B
  class C
  class F[X]

  def f(implicit aa: F[A]) = println(aa)

  implicit def a : F[A] = new F[A]()
  implicit def b[X <: B] = new F[X]()

  f
}
/* bug:
error: ambiguous implicit values:
 both method b in object Test1 of type [X <: Test1.B]Test1.F[X]
 and method a in object Test1 of type => Test1.F[Test1.A]
 match expected type Test1.F[Test1.A]
*/
