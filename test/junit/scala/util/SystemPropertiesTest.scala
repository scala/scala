package scala.util

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class SystemPropertiesTest {
  @Test
  def filterAll(): Unit = {
    val isEmpty = sys.props.filter(_ => false).size == 0
    assertTrue(isEmpty, "A filter matching nothing should produce an empty result")
  }

  @Test
  def filterNone(): Unit = {
    val isUnchanged = sys.props.filter(_ => true) == sys.props
    assertTrue(isUnchanged, "A filter matching everything should not change the result")
  }

  @Test
  def empty(): Unit = {
    val hasSize0 = sys.props.empty.size == 0
    assertTrue(hasSize0, "SystemProperties.empty should have size of 0")
  }
}
