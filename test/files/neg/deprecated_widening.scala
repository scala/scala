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

  // literals don't get a pass -- *especially* literals!

  // 0x7ffffffc0 - 0x7fffffff
  // Set[Float](2147483584, 2147483645, 2147483646, 2147483647)
  def literals  = Set[Float](0x7fffffc0, 0x7ffffffd, 0x7ffffffe, 0x7fffffff)
  def longingly = Set[Float](0x7fffffc0L, 0x7ffffffdL, 0x7ffffffeL, 0x7fffffffL)

  def `pick one` = Set[Float](0x1000003, 0x1000004, 0x1000005)

  def `lossy arg`       = 1f + 2147483584
  def `lossy receiver`  = 2147483584 + 1f

  // currently does not warn
  def f = 1f
  def `lossy arg to overload` = f + 2147483584

}
