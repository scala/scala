package scala.reflect

import org.junit.Assert._
import org.junit.Test

class FieldAccessTest {

  class TestClass {
    private val x = 123
    def fn = () => x
  }

  /** scala/bug#9306 */
  @Test
  def testFieldAccess(): Unit = {
    import scala.reflect.runtime.currentMirror
    import scala.reflect.runtime.universe._
    val obj = new TestClass
    val objType = currentMirror.reflect(obj).symbol.toType
    val objField = objType.member(TermName("x")).asTerm
    assertEquals(123, currentMirror.reflect(obj).reflectField(objField).get)
  }
}
