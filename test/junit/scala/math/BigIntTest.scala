package scala.math

import java.util.concurrent.atomic.AtomicLong
import org.junit.Test
import org.junit.Assert.{assertFalse, assertNull, assertTrue}
import scala.tools.testkit.AssertUtil.assertThrows

class BigIntTest {

  private val counter = new AtomicLong(Int.MaxValue)
  private def bigint = BigInt(counter.getAndIncrement)
  private def even = BigInt(42)

  @Test def testIsComparable: Unit = assertTrue(bigint.isInstanceOf[java.lang.Comparable[_]])

  @Test def `mod respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint mod BigInt(-3), _.contains("modulus not positive"))

  @Test def `modPow respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint.modPow(BigInt(1), BigInt(-3)), _.contains("modulus not positive"))

  @Test def `modInverse respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint.modInverse(BigInt(-3)), _.contains("modulus not positive"))

  @Test def `pow respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint pow -2, _.contains("Negative exponent"))

  @Test def `% respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint % 0, _.contains("/ by zero"))

  @Test def `setBit respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint setBit -1, _.contains("Negative bit address"))

  @Test def `clearBit respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint clearBit -1, _.contains("Negative bit address"))

  @Test def `flipBit respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint flipBit -1, _.contains("Negative bit address"))

  @Test def `/ respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint / BigInt(0), _.contains("/ by zero"))

  @Test def `/% respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint /% BigInt(0), _.contains("/ by zero"))

  @Test def `testBit respects BigInteger`: Unit = assertThrows[ArithmeticException](bigint.testBit(-3), _.contains("Negative bit address"))

  @Test def `testBit 0`: Unit = assertFalse(even.testBit(0))

  @Test def `BitInteger to BitInt respects null`: Unit = assertNull(null.asInstanceOf[java.math.BigInteger]: BigInt)
}
