trait A {
  type T <: A;
  def baz(): A;
  def bar(): T;
  def foo1 = bar().bar();
  def foo2 = bar().baz();
  def foo3 = baz().bar();
  def foo4 = baz().baz();
}
