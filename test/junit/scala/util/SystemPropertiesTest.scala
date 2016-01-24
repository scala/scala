package scala.util

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class SystemPropertiesTest {
  @Test
  def filterAll(): Unit = {
    val isEmpty = sys.props.filter(_ => false).size == 0
    assertTrue("A filter matching nothing should produce an empty result", isEmpty)
  }

  @Test
  def filterNone(): Unit = {
    val isUnchanged = sys.props.filter(_ => true) == sys.props
    assertTrue("A filter matching everything should not change the result", isUnchanged)
  }

  @Test
  def empty(): Unit = {
    val hasSize0 = sys.props.empty.size == 0
    assertTrue("SystemProperties.empty should have size of 0", hasSize0)
  }
}
