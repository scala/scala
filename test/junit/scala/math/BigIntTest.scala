package scala.math

import java.math.{BigInteger => BI, MathContext => MC}

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class BigIntTest {

  @Test
  def testIsComparable() {
    assert(BigInt(1).isInstanceOf[java.lang.Comparable[_]])
  }
}
