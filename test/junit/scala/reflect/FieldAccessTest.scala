package scala.reflect

import org.junit.Assert._
import org.junit.Test

class FieldAccessTest {

  class TestClass {
    private val x = 123
    locally {
      () => x
    }
  }

  /** scala/bug#9306 */
  @Test
  def testFieldAccess(): Unit = {
    import scala.reflect.runtime.universe._
    import scala.reflect.runtime.currentMirror
    val obj = new TestClass
    val objType = currentMirror.reflect(obj).symbol.toType
    val objFields = objType.members.collect { case ms: MethodSymbol if ms.isGetter => ms }
    assertEquals(123, currentMirror.reflect(obj).reflectField(objFields.head).get)
  }
}
