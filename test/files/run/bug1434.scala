object Test {
  class A[T] { val op = null }
  class B extends A[Any]
  class C extends B

  def f(o: AnyRef) = o match {
    case a: A[_] if(a.op != null) => "with op"
    case c: C => "C"
    case b: B => "B"
  }

  def main(args: Array[String]) = {
    assert("C" == f(new C))
  }
}
