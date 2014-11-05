// private types and terms of companion module are
// available in scope of ctor params.
// before 2.10.1, class B in object A cannot be accessed in object A
object A {
  private class B
  private val b: B = new B
  private type C = Int
  def apply(): A = new A()
}
// if not private, then default arg results in:
// private class B escapes its defining scope as part of type A.B
class A private (b: A.B = A.b, c: A.C = 42)

object C {
  private class B
}
class C(b: C.B)
