package scala.math

import org.junit.Test

class BigIntTest {

  @Test
  def testIsComparable(): Unit =
    assert(BigInt(1).isInstanceOf[java.lang.Comparable[_]])
}
