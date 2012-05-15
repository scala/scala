class A[T]
class B {
  def m(a: A[this.type] = new A[this.type]) { }
}

class C {
  def foo(a: Int, b: Int = 0) = 0
  def foo() = 0
}

object Test {
  def newB = new B
  newB.m()

  val stableB = new B
  stableB.m()

  def f {
    println((new C).foo(0))
  }
}
