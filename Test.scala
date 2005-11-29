object Test extends Application {
  if (1 == 0) throw new Error();
  bar;
  def bar = foo;
  def foo = if (true)
    throw new Error();
}
