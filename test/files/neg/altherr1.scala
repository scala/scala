case class Foo[a]();

module Bug {
  def foo[a](): Foo[a] = foo[a]();
  foo() match { case _ => 0 }
}

