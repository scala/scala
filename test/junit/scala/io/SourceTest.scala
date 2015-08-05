
package scala.io

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

import java.io.{ Console => _, _ }

@RunWith(classOf[JUnit4])
class SourceTest {

  private implicit val `our codec` = Codec.UTF8
  private val charSet = Codec.UTF8.charSet.name

  private def sampler = """
    |Big-endian and little-endian approaches aren't
    |readily interchangeable in general, because the
    |laws of arithmetic send signals leftward from
    |the bits that are "least significant."
    |""".stripMargin.trim

  private def in = new ByteArrayInputStream(sampler.getBytes)

  @Test def canIterateLines() = {
    assertEquals(sampler.lines.size, (Source fromString sampler).getLines.size)
  }
  @Test def loadFromResource() = {
    val res = Source.fromResource("rootdoc.txt")
    assertTrue("No classpath resource found", res.getLines().size > 5)
  }
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
      val c = s.next()
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
      override def next = {
        _ch = iter.next()
        _pos += 1
        _ch
      }
    }
    val s = new CapitalReporting(in)
    // skip to next line and report an error
    do {
      val c = s.next()
    } while (s.ch != '\n')
    s.next()
    val out = new ByteArrayOutputStream
    val ps  = new PrintStream(out, true, charSet)
    s.reportError(s.pos, "That doesn't sound right.", ps)
    assertEquals("0030: THAT DOESN'T SOUND RIGHT.", out.toString(charSet))
  }
}
