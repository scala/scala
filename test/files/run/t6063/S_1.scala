package foo

abstract class Foo {
  private[foo] def f1   = 1
  private def f2        = 2
  protected[foo] def f3 = 3
  protected def f4      = 4
  def f5                = 5
}

object Ob extends Foo
