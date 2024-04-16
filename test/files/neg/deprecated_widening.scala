//> using options -Werror -Xlint:deprecation
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
  val losslessPosFloat:Float = 16777218L  // OK -- lossless
  val negFloat: Float = - 16777216L // OK
  val truncatedNegFloat: Float = - 16777217L // deprecated
  val losslessNegFloat: Float = - 16777218L // OK -- lossless

  val posFloatI:Float = 16777216  // OK
  val truncatedPosFloatI:Float = 16777217  // deprecated
  val losslessPosFloatI:Float = 16777218  // OK -- lossless
  val negFloatI: Float = - 16777216 // OK
  val truncatedNegFloatI: Float = - 16777217 // deprecated
  val losslessNegFloatI: Float = - 16777218 // OK -- lossless

  val posDouble:Double = 18014398509481984L// OK
  val truncatedPosDouble:Double = 18014398509481985L // deprecated
  val losslessPosDouble:Double = 18014398509481988L // OK -- lossless
  val negDouble: Double = - 18014398509481984L // OK
  val truncatedNegDouble: Double = - 18014398509481985L // deprecated
  val losslessNegDouble: Double = - 18014398509481988L // OK -- lossless

  // literals don't get a pass -- *especially* literals!

  // 0x7ffffffc0 - 0x7fffffff
  // Set[Float](2147483584, 2147483645, 2147483646, 2147483647)
  def literals  = Set[Float](0x7fffffc0, 0x7ffffffd, 0x7ffffffe, 0x7fffffff)
  def longingly = Set[Float](0x7fffffc0L, 0x7ffffffdL, 0x7ffffffeL, 0x7fffffffL)

  def `pick one` = Set[Float](0x1000003, 0x1000004, 0x1000005)

  def `no warn` = 1f + 2147483584
  def `no warn either` = 2147483584 + 1f
  def f = 1f
  def `no warn sowieso` = f + 2147483584
}
