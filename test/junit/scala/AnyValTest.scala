package scala

import scala.tools.reflect.ToolBoxError
import scala.tools.testing.RunTesting

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class AnyValTest extends RunTesting {
  @Test(expected = classOf[ToolBoxError]) def testNoBoolSelf: Unit = runner.run("true.self")
  @Test(expected = classOf[ToolBoxError]) def testNoByteSelf: Unit = runner.run("1.toByte.self")
  @Test(expected = classOf[ToolBoxError]) def testNoCharSelf: Unit = runner.run("'A'.self")
  @Test(expected = classOf[ToolBoxError]) def testNoDoubleSelf: Unit = runner.run("1D.self")
  @Test(expected = classOf[ToolBoxError]) def testNoFloatSelf: Unit = runner.run("1F.self")
  @Test(expected = classOf[ToolBoxError]) def testNoIntSelf: Unit = runner.run("1.self")
  @Test(expected = classOf[ToolBoxError]) def testNoLongSelf: Unit = runner.run("1L.self")
  @Test(expected = classOf[ToolBoxError]) def testNoShortSelf: Unit = runner.run("1.toShort.self")
}
