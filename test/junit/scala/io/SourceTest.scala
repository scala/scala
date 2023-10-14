
package scala.io

import org.junit.Test
import org.junit.Assert.assertEquals

import scala.tools.testkit.AssertUtil.{assertThrows, fail}

import java.io.{Console => _, _}

class SourceTest {

  private implicit val `our codec`: Codec = Codec.UTF8
  private val charSet = Codec.UTF8.charSet.name

  private def sampler = """
    |Big-endian and little-endian approaches aren't
    |readily interchangeable in general, because the
    |laws of arithmetic send signals leftward from
    |the bits that are "least significant."
    |""".stripMargin.trim

  private def in = new ByteArrayInputStream(sampler.getBytes)

  @Test def canIterateLines() = assertEquals(sampler.linesIterator.size, (Source fromString sampler).getLines().size)
  @Test def loadFromResource() = {
    val res = Source.fromResource("rootdoc.txt")
    val ls = res.getLines()
    ls.next() match {
      case "The Scala compiler and reflection APIs." =>
      case "This is the documentation for the Scala standard library." =>
      case l => fail(s"$l\n${ls.mkString("\n")}")
    }
  }
  @Test def loadFromMissingResource(): Unit = assertThrows[FileNotFoundException](Source.fromResource("missing.txt"))
  @Test def canCustomizeReporting() = {
    class CapitalReporting(is: InputStream) extends BufferedSource(is) {
      override def report(pos: Int, msg: String, out: PrintStream): Unit = {
        out print f"$pos%04x: ${msg.toUpperCase}"
      }
      class OffsetPositioner extends Positioner(null) {
        override def next(): Char = {
          ch = iter.next()
          pos = pos + 1
          ch
        }
      }
      withPositioning(new OffsetPositioner)
    }
    val s = new CapitalReporting(in)
    // skip to next line and report an error
    do {
      s.next()
    } while (s.ch != '\n')
    s.next()
    val out = new ByteArrayOutputStream
    val ps  = new PrintStream(out, true, charSet)
    s.reportError(s.pos, "That doesn't sound right.", ps)
    assertEquals("0030: THAT DOESN'T SOUND RIGHT.", out.toString(charSet))
  }
  @Test def canAltCustomizeReporting() = {
    class CapitalReporting(is: InputStream)(implicit codec: Codec) extends Source {
      override val iter = {
        val r = new InputStreamReader(is, codec.decoder)
        Iterator continually (codec wrap r.read()) takeWhile (_ != -1) map (_.toChar)
      }
      override def report(pos: Int, msg: String, out: PrintStream): Unit = {
        out print f"$pos%04x: ${msg.toUpperCase}"
      }
      private[this] var _pos: Int = _
      override def pos = _pos
      private[this] var _ch: Char = _
      override def ch = _ch
      override def next() = {
        _ch = iter.next()
        _pos += 1
        _ch
      }
    }
    val s = new CapitalReporting(in)
    // skip to next line and report an error
    do {
      s.next()
    } while (s.ch != '\n')
    s.next()
    val out = new ByteArrayOutputStream
    val ps  = new PrintStream(out, true, charSet)
    s.reportError(s.pos, "That doesn't sound right.", ps)
    assertEquals("0030: THAT DOESN'T SOUND RIGHT.", out.toString(charSet))
  }
  @Test def `t8690 mkString uses correct iterator and sees first char`: Unit = {
    val txt = "abcdef"
    val source = Source.fromInputStream(new ByteArrayInputStream(txt.getBytes(charSet)))
    assertEquals("<iterator>", source.toString) // forces the BufferedSource to look at the head of the input
    assertEquals(txt, source.mkString)          // previously returned "bcdef" ...
  }
  @Test def `t2104 mkString checks for EOF and sees last char`: Unit = {
    val N = 4
    val chars = List('\n','\r','a')
    def allStrings(n: Int): List[List[Char]] =
      if (n==0) List(Nil)
      else {
        val sufs = allStrings(n-1)
        chars.flatMap((c) => sufs.map(c :: _))
      }
    def test(n: Int): Unit =
      for (cs <- allStrings(n)) {
        val source = Source.fromInputStream(new ByteArrayInputStream(cs.mkString.getBytes(charSet)))
        assertEquals(cs, source.toList)
      }
    (0 until N).foreach(test(_))
  }
}
