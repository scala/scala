package scala.runtime

import org.junit.Test

import scala.collection.immutable.NumericRange
import scala.tools.testkit.AssertUtil.assertThrows

class RichCharTest {
  @Test
  def rangeReverse(): Unit = {
    def check(range: NumericRange[Char]): Unit =
      assertThrows[ArithmeticException](range.reverse,
                                        s => Seq("unsigned", "reverse", "negative step").forall(s.contains))

    check('a' until 'z')
    check('a' until 'z' by 2.toChar)
    check('a' to 'z')
    check('a' to 'z' by 2.toChar)
  }
}
