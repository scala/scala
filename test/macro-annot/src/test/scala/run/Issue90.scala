import org.junit._
import org.junit.runner._
import org.junit.runners._
import Assert._
import scala.reflect.runtime.{currentMirror => cm}

@RunWith(classOf[JUnit4])
class Issue90 {
  @Test
  def compileTimeOnlyOnceOnly: Unit = {
    assertEquals(cm.staticClass("issue90Class").annotations.length, 1)
  }

  @Test
  def compileTimeOnlyOnceOnlyMessage: Unit = {
    assertEquals(cm.staticClass("issue90Class").annotations.head.toString, "scala.annotation.compileTimeOnly(\"this is the only annotation\")")
  }
}
