// This is extracted from a test file => don't add a new test file.
object bug {
  def foo(i: => Int): Int = 0;

  def bar: Int = {
    var i: Int = 0;
    foo (i);
  }
}
