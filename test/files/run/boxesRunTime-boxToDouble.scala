// partest test/files/run/boxesRunTime-boxToDouble.scala
object Test {
  def main(args: Array[String]): Unit = {
    assert(!(-0.0.asInstanceOf[java.lang.Double] eq 0.0.asInstanceOf[java.lang.Double]), "negative zero was positive")

    assert(0.0.asInstanceOf[java.lang.Double] eq 0.0.asInstanceOf[java.lang.Double], "zero was not interned")
    assert(-0.0.asInstanceOf[java.lang.Double] eq -0.0.asInstanceOf[java.lang.Double], "negative zero was not interned")
    assert(1.0.asInstanceOf[java.lang.Double] eq 1.0.asInstanceOf[java.lang.Double], "one was not interned")
    assert(-1.0.asInstanceOf[java.lang.Double] eq -1.0.asInstanceOf[java.lang.Double], "negative one was not interned")

    // a reminder of the java.land.Double API vs Scala/Java primitives
    assert(java.lang.Double.NaN.equals(java.lang.Double.NaN))
    assert(Double.NaN != Double.NaN)

    // canary, the JVM may start to cache these...
    assert(!(100.0.asInstanceOf[java.lang.Double] eq 100.0.asInstanceOf[java.lang.Double]), "100 was interned")
  }
}
