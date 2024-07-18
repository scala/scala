//> using options -Ywarn-numeric-widen -Xfatal-warnings
//
trait Foo

class WarnWidening {
  implicit class FooDouble(d: Double) { def foo = new Foo {} }
  def elapsed: Foo = (System.nanoTime.toInt - 100).foo
}

class NoWarnWidening {
  implicit class FooInt(i: Int) { def foo = new Foo {} }
  implicit class FooDouble(d: Double) { def foo = new Foo {} }
  def elapsed: Foo = (System.nanoTime.toInt - 100).foo
}
