// eta expansion to Function0 is problematic (as shown here).
// Perhaps we should we deprecate it? See discussion in the comments of
// on SI-9178.
//
// This test encodes the status quo: no deprecation.
object Test {
  def foo(): () => String = () => ""
  val f: () => Any = foo

  def main(args: Array[String]): Unit = {
    println(f()) // <function0>
  }
}
