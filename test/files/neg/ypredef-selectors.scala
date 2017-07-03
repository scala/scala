object StringPlusConfusion {
  println(???)
  def f[T](x: T) = x + 5

  // After we block out any2stringadd, the error is:
  //
  // work/a.scala:7: error: value + is not a member of type parameter T
  //   def f[T](x: T) = x + 5
  //                      ^
  // Normally, it is:
  //
  // work/a.scala:7: error: type mismatch;
  //  found   : Int(5)
  //  required: String
  //   def f[T](x: T) = x + 5
  //                        ^
}
