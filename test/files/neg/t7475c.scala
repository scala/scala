class A {
  private val a: Int = 0
  private[this] val b: Int = 0
  class B extends A {
    def foo(a: A) = a.a // okay
    println(this.a)     // wait, what?
    println(this.b)     // wait, what?
  }
}
