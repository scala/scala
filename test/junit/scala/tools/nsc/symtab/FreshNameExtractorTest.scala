package scala.tools.nsc
package symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil.assertThrows
import scala.reflect.internal.util.FreshNameCreator

@RunWith(classOf[JUnit4])
class FreshNameExtractorTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  val prefixes = List("foo$", "x$", "bar", "bippy$baz$")

  @Test
  def extractionPreservesPrefix =
    ("" :: prefixes).foreach { creatorPrefix =>
      prefixes.foreach { newPrefix =>
        val Creator = new FreshNameCreator(creatorPrefix)
        val Extractor = new FreshNameExtractor(creatorPrefix)
        val Extractor(extractedPrefix) = TermName(Creator.newName(newPrefix))
        assertEquals(newPrefix, extractedPrefix)
      }
    }

  @Test
  def extractionFailsOnCreatorPrefixMismatch = {
    val Creator = new FreshNameCreator(prefixes.head)
    val Extractor = new FreshNameExtractor(prefixes.tail.head)
    assertThrows[MatchError] {
      val Extractor(_) = TermName(Creator.newName("foo"))
    }
  }

  @Test @org.junit.Ignore // SI-8818
  def extractionsFailsIfNameDoesntEndWithNumber = {
    val Creator = new FreshNameCreator(prefixes.head)
    val Extractor = new FreshNameExtractor(prefixes.head)
    assertThrows[MatchError] {
      val Extractor(_) = TermName(Creator.newName("foo") + "bar")
    }
  }
}
