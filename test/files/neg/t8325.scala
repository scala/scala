
trait Test {
  type OK[A,B] = A Tuple2 B
  type *[A,B]  = A Tuple2 B
  def f(is: Int*, s: String) = ???
  def g(is: Int * String, s: String) = ???     // OK
  def h(is: Int * String *, s: String) = ???
  // won't recover from following
  //def i(is: Int OK) = ???  //error: identifier expected but ')' found.
  def j(is: Int* = 5) = ???
}
