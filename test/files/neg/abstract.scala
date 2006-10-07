trait A {
  type T <: A;
  def baz(): A;
  def bar(): T;
  def foo1: A = bar().bar();
  def foo2: T = bar().baz();
  def foo3 = bar().baz();
  def foo4: A = baz().bar();
  def foo5: T = baz().baz();
  def foo6 = baz().baz();
}
