
package scala.reflect

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

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
