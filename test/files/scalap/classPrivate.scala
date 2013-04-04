class ClassPrivate {
  private def foo = 1
  private[ClassPrivate] def bar = 2
  def baz = 3
  class Outer {
    private[ClassPrivate] def qux = 4
  }
  protected def quux = 5
}