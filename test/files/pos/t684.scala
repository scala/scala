package test;
trait Test {
  trait Ti;
  class Foo;
  def foo(t : Ti) = t match {
  case t : Foo => true;
  case _ => false;
  }
  class Bar extends Foo with Ti;
  assert(foo(new Bar));
}
