import org.junit._
import org.junit.runner._
import org.junit.runners._
import Assert._
import scala.reflect.runtime.{currentMirror => cm}

@RunWith(classOf[JUnit4])
class Definition {
  @Test
  def macroAnnotationsMarkedWithMACRO: Unit = {
    assertEquals(cm.staticClass("identity").isMacro, true)
  }
}