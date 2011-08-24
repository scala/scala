trait Foo {
  def foobar: Unit;
}

trait Bar extends Foo {
  def foobar: Unit = super.foobar
}

// the following definition breaks the compiler
abstract class Foobar extends Bar
