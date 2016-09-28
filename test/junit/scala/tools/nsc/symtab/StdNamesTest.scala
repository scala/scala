package scala.tools.nsc
package symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class StdNamesTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._
  import nme.{SPECIALIZED_SUFFIX, splitSpecializedName, unspecializedName}

  @Test
  def testNewTermNameInvalid(): Unit = {
    assertThrows[IllegalArgumentException](newTermName("foo".toCharArray, -1, 1))
  }

  @Test
  def testNewTermNameNegativeLength(): Unit = {
    assertEquals(nme.EMPTY, newTermName("foo".toCharArray, 0, -1))
    assertEquals(nme.EMPTY, newTermName("foo".toCharArray, 0, 0))
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
