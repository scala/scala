object Test extends App {
  // #2235
  new A2235 with B2235
}

// only one overloaded alternative is allowed to have defaults
class A {
  def foo(a: Int = 0) = a
  def foo(b: String = "another") = b
}

class B {
  def foo(a: Int) = a
  def bar(u: String = "ldksj") = u
}

class C extends B {
  override def foo(a: Int = 1092) = a
  def foo(b: String = "lskdfj")

  def bar(i: Int = 129083) = i
}

// #2235
trait A2235 { def f(x: Int = 1) = x }
trait B2235 { def f(x: String = "1") = x }
