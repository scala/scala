object Test extends App {

  def foo(is:Int*) = 1;
  def foo(i:Int) = 2;

  assert(foo( List(3):_* ) == 1)
}
