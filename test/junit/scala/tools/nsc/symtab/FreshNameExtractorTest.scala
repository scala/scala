package scala.tools.nsc
package symtab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.reflect.internal.util.FreshNameCreator
import scala.tools.testkit.AssertUtil.assertThrows

class FreshNameExtractorTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  val prefixes = List("foo$", "x$", "bar", "bippy$baz$")

  @Test
  def extractionPreservesPrefix(): Unit =
    ("" :: prefixes).foreach { creatorPrefix =>
      prefixes.foreach { newPrefix =>
        val Creator = new FreshNameCreator(creatorPrefix)
        val Extractor = new FreshNameExtractor(creatorPrefix)
        val Extractor(extractedPrefix) = TermName(Creator.newName(newPrefix))
        assertEquals(newPrefix, extractedPrefix)
      }
    }

  @Test
  def extractionFailsOnCreatorPrefixMismatch(): Unit = {
    val Creator = new FreshNameCreator(prefixes.head)
    val Extractor = new FreshNameExtractor(prefixes.tail.head)
    assertThrows[MatchError] {
      TermName(Creator.newName("foo")) match { case Extractor(_) => }
    }
  }

  @Test
  def `no numeric suffix? no problem!`(): Unit = {
    val Creator   = new FreshNameCreator(prefixes.head)
    val Extractor = new FreshNameExtractor(prefixes.head)
    TermName(Creator.newName("foo") + "bar") match {
      case Extractor(_) =>
    }
  }
}
