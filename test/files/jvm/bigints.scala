//############################################################################
// BigInt, BigDecimal 
//############################################################################

//############################################################################

import testing.SUnit._

/** Test the Scala implementation of class <code>scala.BigDecimal</code>.
*
*  @author Stephane Micheloud
*/
object Test extends TestConsoleMain {
  def suite = new TestSuite(
    Test_BigInt,
    Test_BigDecimal
  )
}

object Test_BigInt extends TestCase("BigInt") with Assert {
  override def enableStackTrace = false
  override def runTest {
    import BigInt._

    val x: BigInt = 1
    val y = x + 1
    val z = 1 + y
    assertEquals("int_add_bigint", 1+y, y+1)
    assertEquals("int_sub_bigint", 1-y, -(y-1))
    assertEquals("int_mul_bigint", 2*x*y, y*x*2)
    assertTrue("z_<=_3", z <= 3)
    assertFalse("3_<_z", 3 < z)
  }
}

object Test_BigDecimal extends TestCase("BigDecimal") with Assert {
  override def enableStackTrace = false
  override def runTest {
    import scala.BigDecimal, BigDecimal._

    val xi: BigDecimal = 1
    val xd: BigDecimal = 1.0
    val xf: BigDecimal = BigDecimal(1.0f)
    val xs: BigDecimal = BigDecimal("1.0")
    val xbi: BigDecimal = BigDecimal(scala.BigInt(1))

    val x: BigDecimal = 1
    val y = x + 1
    val z = 1 + y
    assertTrue("z_<=_3", z <= 3)
    assertFalse("3_<_z", 3 < z)

    val a: BigDecimal= Math.MAX_LONG
    val b: BigDecimal = 1
    val c = a - b
    assertFalse("c_>_MAX_LONG", c > Math.MAX_LONG)
    assertTrue("c_<=_MAX_LONG", c <= Math.MAX_LONG)
  }
}

