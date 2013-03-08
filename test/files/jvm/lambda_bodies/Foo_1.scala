class Foo_1 {
  def baz(f: Int => Int, x: Int) = f(x)
  def bar {
    // magic number '3' should only appear in this lambda
    baz(x => x + 3, 1)
  }
}