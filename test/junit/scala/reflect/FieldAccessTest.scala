package scala.reflect

import org.junit.Assert._
import org.junit.Ignore
import org.junit.Test

class FieldAccessTest {

  class TestClass {
    private val x = 123
    // Uncommenting the following line would make the test fail
    () => x
  }

  /** scala/bug#9306 */
  @Test
  def testFieldAccess(): Unit = {
    import scala.reflect.runtime.{universe => ru}
    import scala.reflect.runtime.currentMirror
    val obj = new TestClass
    val objType = currentMirror.reflect(obj).symbol.toType
    val objFields = objType.members.collect { case ms: ru.MethodSymbol if ms.isGetter => ms }
    assertEquals(123, currentMirror.reflect(obj).reflectField(objFields.head).get)
  }
}
