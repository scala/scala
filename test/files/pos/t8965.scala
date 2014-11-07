class A {
  def f(x: Any with AnyRef, y: Any with AnyRef) = x eq y
  // a.scala:2: warning: Any and Any are unrelated: they will most likely never compare equal
  //   def f(x: Any with AnyRef, y: Any with AnyRef) = x eq y
  //                                                     ^
  // one warning found
}
