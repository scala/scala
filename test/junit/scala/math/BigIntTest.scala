package scala.math

import org.junit.Test

class BigIntTest {

  @Test
  def testIsComparable(): Unit =
    assert(BigInt(1).isInstanceOf[java.lang.Comparable[_]])

  @Test def exponentiationOperator(): Unit = assert( BigInt(2)\3 == BigInt(8) )

}
