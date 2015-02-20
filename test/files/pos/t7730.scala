// Fails
class Foo {
  def f(x: Any) = x match { case x: (T forSome { type T }) => x }
  // a.scala:2: error: not found: type T
  //   def f(x: Any) = x match { case x: (T forSome { type T }) => x }
  //                                      ^
}
