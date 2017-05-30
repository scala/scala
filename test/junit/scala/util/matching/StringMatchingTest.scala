
package scala.util.matching

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.annotation.switch

@RunWith(classOf[JUnit4])
class StringMatchingTest {
  
  @Test def StringMatch(): Unit = {
  def getnum(str: String) = (str: @switch) match {
    case "foo" => 1
    case "bar" => 2
    case "baz" => 3
    case "quuz" => 4
    case _ => 0
  }
    assertEquals(1, getnum("foo"))
    assertEquals(2, getnum("bar"))
    assertEquals(3, getnum("baz"))
    assertEquals(4, getnum("quuz"))
    assertEquals(0, getnum("other"))
  }

}
