object Test {
  case class CC[T](key: T)
  type Alias[T] = Seq[CC[T]]

  def f(xs: Seq[CC[_]]) = xs map { case CC(x) => CC(x) }    // ok
  def g(xs: Alias[_])   = xs map { case CC(x) => CC(x) }    // fails
  // ./a.scala:11: error: missing parameter type for expanded function
  // The argument types of an anonymous function must be fully known. (SLS 8.5)
  // Expected type was: ?
  //   def g(xs: Alias[_])   = xs map { case CC(x) => CC(x) }    // fails
  //                                  ^
  // one error found
}