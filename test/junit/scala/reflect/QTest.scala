
package scala.reflect

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class QTest {

  import reflect.runtime._
  import universe._
  @Test def qConstantsNotHomogenized() = {
    //Apply(Select(Literal(Constant(1.0)), TermName("$plus")), List(Literal(Constant(1.0))))
    val t = q"${1} + ${1.0}"
    val Apply(Select(Literal(Constant(i)), TermName("$plus")), List(Literal(Constant(j)))) = t
    assertEquals(1, i)
    assertEquals(1.0, j)
  }
}
