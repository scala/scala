object Test {
  def confirmSame(x: Any)       = assert(x.## == x.hashCode, "%s.## != %s.hashCode".format(x, x))
  def confirmDifferent(x: Any)  = assert(x.## != x.hashCode, "%s.## == %s.hashCode (but should not)".format(x, x))

  def main(args: Array[String]): Unit = {
    /** Just a little sanity check, not to be confused with a unit test. */
    List(5, 5.5f, "abc", new AnyRef, ()) foreach confirmSame
    List(5.0f, 1.0d, -(5.0f), (-1.0d)) foreach confirmDifferent

    val x = (BigInt(1) << 64).toDouble
    val y: Any = x
    val f: Float = x.toFloat
    val jn: java.lang.Number = x
    val jf: java.lang.Float = x.toFloat
    val jd: java.lang.Double = x

    assert(x.## == y.##, ((x, y)))
    assert(x.## == f.##, ((x, f)))
    assert(x.## == jn.##, ((x, jn)))
    assert(x.## == jf.##, ((x, jf)))
    assert(x.## == jd.##, ((x, jd)))
  }
}
