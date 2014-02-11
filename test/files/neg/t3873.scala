class A {
  class B
  def b: B = new B
}

object Test {
  def wrongf(a: A)(b: a.B): a.B = b

  val a = new A
  wrongf(a)(a.b)
  wrongf(new A)(a.b) // should not compile
}