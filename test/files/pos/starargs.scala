case class C[a](x: a*) {
  def elems: Seq[a] = x;
}

object Test with Executable {
  def foo(x: int*) = {
    C(x: _*);
  }
  System.out.println(foo(1, 2, 3).elems);
  System.out.println(foo(List(1, 2, 3): _*).elems);
  System.out.println(new C(1, 2, 3).elems);
  val xs = List(1, 2, 3);
  System.out.println(new C(xs: _*).elems);
}



