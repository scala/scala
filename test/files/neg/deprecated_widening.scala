// scalac: -deprecation -Werror
//
object Test {
  def foo(i: Int, l: Long): Unit = {
    val i_f: Float = i  // deprecated
    val i_d: Double = i // OK
    val l_f: Float = l  // deprecated
    val l_d: Double = l // deprecated
  }

  def imp: Unit = {
    implicitly[Int => Float]   // deprecated
    implicitly[Int => Double]  // OK
    implicitly[Long => Float]  // deprecated
    implicitly[Long => Double] // deprecated
  }

  // don't leak silent warning from float conversion
  val n = 42
  def clean = n max 27

  val posFloat:Float = 16777216L  // OK
  val truncatedPosFloat:Float = 16777217L  // deprecated
  val negFloat: Float = - 16777216L // OK
  val truncatedNegFloat: Float = - 16777217L // deprecated

  val posFloatI:Float = 16777216  // OK
  val truncatedPosFloatI:Float = 16777217  // deprecated
  val negFloatI: Float = - 16777216 // OK
  val truncatedNegFloatI: Float = - 16777217 // deprecated

  val posDouble:Double = 18014398509481984L// OK
  val truncatedPosDouble:Double = 18014398509481985L // deprecated
  val negDouble: Double = - 18014398509481984L // OK
  val truncatedNegDouble: Double = - 18014398509481985L // deprecated
}
