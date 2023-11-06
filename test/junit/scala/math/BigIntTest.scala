package scala.math


import org.junit.Assert.assertNull
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class BigIntTest {

  @Test
  def testIsComparable() {
    assert(BigInt(1).isInstanceOf[java.lang.Comparable[_]])
  }

  @Test def `BitInteger to BitInt respects null`: Unit = assertNull(null.asInstanceOf[java.math.BigInteger]: BigInt)
}
