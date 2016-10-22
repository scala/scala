// partest test/files/run/boxesRunTime-boxToDouble.scala
object Test {
  def main(args: Array[String]): Unit = {
    assert(0.0.asInstanceOf[java.lang.Double] eq 0.0.asInstanceOf[java.lang.Double], "zero was not interned")
    assert(-0.0.asInstanceOf[java.lang.Double] eq -0.0.asInstanceOf[java.lang.Double], "negative zero was not interned")
    assert(1.0.asInstanceOf[java.lang.Double] eq 1.0.asInstanceOf[java.lang.Double], "one was not interned")
    assert(-1.0.asInstanceOf[java.lang.Double] eq -1.0.asInstanceOf[java.lang.Double], "negative one was not interned")

    // a reminder of the java.lang.Double vs primitive equality exceptions
    assert(java.lang.Double.NaN.equals(java.lang.Double.NaN))
    assert(Double.NaN != Double.NaN)

    assert(!(java.lang.Double.valueOf(0.0).equals(java.lang.Double.valueOf(-0.0))))
    assert(0.0 == -0.0)
    assert(!(-0.0.asInstanceOf[java.lang.Double] eq 0.0.asInstanceOf[java.lang.Double]), "negative zero was positive")
  }
}
