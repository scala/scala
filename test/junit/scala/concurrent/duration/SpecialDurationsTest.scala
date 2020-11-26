package scala.concurrent.duration

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class SpecialDurationsTest {
  @Test
  def test_11178(): Unit = {
    assert(Duration(Duration.Inf.toString) eq Duration.Inf)
    assert(Duration(Duration.MinusInf.toString) eq Duration.MinusInf)
    assert(Duration(Duration.Undefined.toString) eq Duration.Undefined)
  }
}
