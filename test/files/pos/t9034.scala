object Test {
  abstract class Foo {
    protected val foo: Boolean
  }

  def foo[@specialized T](t: T) = new Foo {
    protected val foo: Boolean = true
  }
}
