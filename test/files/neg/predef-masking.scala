// Testing predef masking
import Predef.{ any2stringadd => _, _ }

object StringPlusConfusion {
  // Would love to do something about this error message, but by the
  // time we get our hands on it the context is lost.
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
