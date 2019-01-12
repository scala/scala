
package scala.collection

import java.io.{BufferedReader, StringReader}

import org.scalacheck.{Gen, Properties}, Gen.{oneOf, listOf}
import org.scalacheck.Prop._

import JavaConverters._

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

  property("when string contains a terminator, WithSeparators is longer") = forAll(line) { s =>
    val hasEOL = s.exists(c => c == '\r' || c == '\n')
    if (hasEOL) s.linesIterator.map(_.length).sum < s.linesWithSeparators.map(_.length).sum
    else s.linesIterator.map(_.length).sum == s.linesWithSeparators.map(_.length).sum
  }
}
