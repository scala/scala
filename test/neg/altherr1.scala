case class Foo[a]();

object Bug {
  def foo[a](): Foo[a] = foo[a]();
  foo() match { case _ => 0 }
}

