
package scala.collection

import java.io.{BufferedReader, StringReader}

import org.scalacheck.{Gen, Properties}, Gen.{oneOf, listOf}
import org.scalacheck.Prop._

import scala.jdk.CollectionConverters._

object StringOpsTest extends Properties("StringOps") {

  val lineChar: Gen[Char] = oneOf('X', '\r', '\n')

  val line = listOf(lineChar).map(_.mkString)

  property("linesIterator tracks BufferedReader.lines") = forAll(line) { s =>
    val r = new BufferedReader(new StringReader(s))

    s.linesIterator.sameElements(r.lines.iterator.asScala)
  }

  property("linesIterator returns stripped lines") = forAll(line) { s =>
    s.linesIterator.sameElements(s.linesWithSeparators.map(_.stripLineEnd))
  }

  property("stripped lines are shorter except maybe last") = forAll(line) { s =>
    val stripped = s.linesIterator.toList
    val lines    = s.linesWithSeparators.toList
    val zipped   = stripped.zip(lines)
    def shorter  = zipped.init.forall { case (s, l) => s.length < l.length }
    def maybe    = zipped.last match { case (s, l) => s.length <= l.length }

    (stripped.length == lines.length && (lines.isEmpty || shorter && maybe))
  }
}
