package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MapViewTest {
  @Test
  def _toString(): Unit = {
    assertEquals("MapView(<not computed>)", Map(1 -> 2).view.toString)
  }
}
