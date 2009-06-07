class A {
  def foo(x: Int) = 0
  def foo(x: String) = 1
}

class ann(x: Int) extends StaticAnnotation

class t {
  val a = new A
  @ann(a.foo(1)) def bar = 1
}
