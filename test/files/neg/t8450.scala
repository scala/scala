trait Foo

class WarnWidening {
  implicit class FooDouble(d: Double) { def foo = new Foo {} }
  def elapsed: Foo = (System.nanoTime - 100L).foo
}

class NoWarnWidening {
  implicit class FooLong(l: Long) { def foo = new Foo {} }
  implicit class FooDouble(d: Double) { def foo = new Foo {} }
  def elapsed: Foo = (System.nanoTime - 100L).foo
}
