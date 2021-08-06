package scala.util

import org.junit.Before
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class PropertiesTest {
  final val TestProperty = "scala.util.PropertiesTest.__test_property__"

  @Before
  def beforeEach(): Unit = {
    Properties.clearProp(TestProperty)
  }

  @Test
  def testPropOrNone(): Unit = {
    assertEquals(Properties.propOrNone(TestProperty), None)

    Properties.setProp(TestProperty, "foo")

    assertEquals(Properties.propOrNone(TestProperty), Some("foo"))
  }

  @Test
  def testPropOrElse(): Unit = {
    assertEquals(Properties.propOrElse(TestProperty, "bar"), "bar")

    Properties.setProp(TestProperty, "foo")

    var done = false
    assertEquals(Properties.propOrElse(TestProperty, { done = true; "bar" }), "foo")
    assertFalse(done, "Does not evaluate alt if not needed")
  }

  @Test
  def testEnvOrElse(): Unit = {
    assertEquals(Properties.envOrElse("_PropertiesTest_NOT_DEFINED", "test"), "test")

    var done = false
    val envName = System.getenv().keySet().iterator().next()
    assertNotEquals(Properties.envOrElse(envName, { done = true; "bar" }), "bar")
    assertFalse(done, "Does not evaluate alt if not needed")
  }
}
