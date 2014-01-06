package scala.tools.nsc
package symtab

import org.junit.Assert._
import scala.tools.testing.AssertUtil._
import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StdNamesTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._
  import nme.{SPECIALIZED_SUFFIX, unspecializedName, splitSpecializedName}

  @Test
  def testNewTermNameInvalid(): Unit = {
    assertThrows[IllegalArgumentException](newTermName("foo".toCharArray, 0, -1))
    assertThrows[IllegalArgumentException](newTermName("foo".toCharArray, 0, 0))
    assertThrows[IllegalArgumentException](newTermName("foo".toCharArray, -1, 1))
  }

  @Test
  def testUnspecializedName(): Unit = {
    def test(expected: Name, nme: Name) {
      assertEquals(expected, unspecializedName(nme))
    }
    test(TermName("Tuple2"), TermName("Tuple2$mcII" + SPECIALIZED_SUFFIX))
    test(TermName("foo"), TermName("foo$mIcD" + SPECIALIZED_SUFFIX))
    test(TermName("foo"), TermName("foo$mIc" + SPECIALIZED_SUFFIX))
    test(nme.EMPTY, TermName(s"T1$SPECIALIZED_SUFFIX"))
    test(nme.EMPTY, SPECIALIZED_SUFFIX)
  }

  @Test
  def testSplitSpecializedName(): Unit = {
    def test(expected: (Name, String, String), nme: Name) {
      assertEquals(expected, splitSpecializedName(nme))
    }
    test((TermName("Tuple2"), "II", ""), TermName("Tuple2$mcII" + SPECIALIZED_SUFFIX))
    test((TermName("foo"), "D", "I"), TermName("foo$mIcD" + SPECIALIZED_SUFFIX))
    test((TermName("foo"), "", "I"), TermName("foo$mIc" + SPECIALIZED_SUFFIX))
    test((nme.EMPTY, "T1", ""), TermName(s"T1$SPECIALIZED_SUFFIX"))
    test((nme.EMPTY, "", ""), SPECIALIZED_SUFFIX)
  }
}
