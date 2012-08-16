object Test extends App {
  // checks whether Double.Epsilon == 2^(-53)
  assert(Double.Epsilon == java.lang.Double.longBitsToDouble(0x3ca0000000000000L))

  // checks whether Float.Epsilon == 2^(-24)
  assert(Float.Epsilon == java.lang.Float.intBitsToFloat(0x33800000))

  // epsd is the greatest power of 2 such that 1+epsd == 1
  // this value should be equivalent to Double.Epsilon
  def epsd = {
    var e: Double = 1
    var f = 1 + e
    while(f > 1) { e = e / 2; f = 1+e }
    e
  }
  assert(epsd == Double.Epsilon)

  // epsf is the greatest power of 2 such that 1+epsf == 1
  // this value should be equivalent to Float.Epsilon
  def epsf = {
    var e: Float = 1
    var f = 1 + e
    while(f > 1) { e = e / 2; f = 1+e }
    e
  }
  assert(epsf == Float.Epsilon)
}
