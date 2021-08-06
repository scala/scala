package scala.util.matching

import org.junit.jupiter.api.Assertions.{assertThrows => _, _}
import org.junit.jupiter.api.Test

import scala.tools.testkit.AssertUtil._

class RegexTest {
  @Test def t8022CharSequence(): Unit = {
    val full = """.*: (.)$""".r
    val text = "   When I use this operator: *"
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val y :: Nil = full.unapplySeq(text: CharSequence).get: @unchecked
    assertEquals("*", y)
  }

  @Test def t8022Match(): Unit = {
    val R = """(\d)""".r
    val matchh = R.findFirstMatchIn("a1").get
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val y :: Nil = R.unapplySeq(matchh).get: @unchecked
    assertEquals("1", y)
  }

  @Test def `t9666: use inline group names`(): Unit = {
    val r = new Regex("a(?<Bee>b*)c")
    val ms = r findAllIn "stuff abbbc more abc and so on"
    assertTrue(ms.hasNext)
    assertEquals("abbbc", ms.next())
    assertEquals("bbb", ms group "Bee")
    assertTrue(ms.hasNext)
    assertEquals("abc", ms.next())
    assertEquals("b", ms group "Bee")
    assertFalse(ms.hasNext)
  }

  @Test def `t9666: use explicit group names`(): Unit = {
    val r = new Regex("a(b*)c", "Bee")
    val ms = r findAllIn "stuff abbbc more abc and so on"
    assertTrue(ms.hasNext)
    assertEquals("abbbc", ms.next())
    assertEquals("bbb", ms group "Bee")
    assertTrue(ms.hasNext)
    assertEquals("abc", ms.next())
    assertEquals("b", ms group "Bee")
    assertFalse(ms.hasNext)
  }

  @Test def `t9666: fall back to explicit group names`(): Unit = {
    val r = new Regex("a(?<Bar>b*)c", "Bee")
    val ms = r findAllIn "stuff abbbc more abc and so on"
    assertTrue(ms.hasNext)
    assertEquals("abbbc", ms.next())
    assertEquals("bbb", ms group "Bee")
    assertEquals("bbb", ms group "Bar")
    assertTrue(ms.hasNext)
    assertEquals("abc", ms.next())
    assertEquals("b", ms group "Bee")
    assertEquals("b", ms group "Bar")
    assertFalse(ms.hasNext)
  }

  type NoGroup = IllegalArgumentException
  type NoMatch = NoSuchElementException
  type NoData  = IllegalStateException

  @Test def `t9666: throw on bad name`(): Unit = {
    assertThrows[NoGroup] {
      val r = new Regex("a(?<Bar>b*)c")
      val ms = r findAllIn "stuff abbbc more abc and so on"
      assertTrue(ms.hasNext)
      ms group "Bee"
    }
    assertThrows[NoGroup] {
      val r = new Regex("a(?<Bar>b*)c", "Bar")
      val ms = r findAllIn "stuff abbbc more abc and so on"
      assertTrue(ms.hasNext)
      ms group "Bee"
    }
    assertThrows[NoGroup] {
      val r = new Regex("a(b*)c", "Bar")
      val ms = r findAllIn "stuff abbbc more abc and so on"
      assertTrue(ms.hasNext)
      ms group "Bee"
    }
  }

  @Test def `t9827 MatchIterator ergonomics`(): Unit = {
    val r = "(ab)(cd)".r
    val s = "xxxabcdyyyabcdzzz"
    assertEquals(3, r.findAllIn(s).start)
    assertEquals(5, r.findAllIn(s).start(2))
    locally {
      val mi = r.findAllIn(s)
      assertTrue(mi.hasNext)
      assertEquals(3, mi.start)
      assertEquals("abcd", mi.next())
      assertEquals(3, mi.start)
      assertTrue(mi.hasNext)
      assertEquals(10, mi.start)
    }
    locally {
      val mi = r.findAllIn(s)
      assertEquals("abcd", mi.next())
      assertEquals(3, mi.start)
      assertEquals("abcd", mi.next())
      assertEquals(10, mi.start)
      assertThrows[NoMatch] { mi.next() }
      assertThrows[NoData] { mi.start }
    }
    locally {
      val mi = r.findAllIn("")
      assertThrows[NoData] { mi.start }
      assertThrows[NoMatch] { mi.next() }
    }
    locally {
      val mi = r.findAllMatchIn(s)
      val x = mi.next()
      assertEquals("abcd", x.matched)
      assertEquals(3, x.start)
      val y = mi.next()
      assertEquals("abcd", y.matched)
      assertEquals(10, y.start)
      assertThrows[NoMatch] { mi.next() }
      assertEquals(3, x.start)
      assertEquals(10, y.start)
    }
    locally {
      val regex = "(foo)-(.*)".r
      val s = "foo-abc-def"
      val result = regex.findAllIn(s)
      //result.toString // comment this line to make it not work
      val r = (result.group(1), result.group(2))
      assertEquals(("foo", "abc-def"), r)
    }
    locally {
      val t = "this is a test"
      val rx = " ".r
      val m = rx.findAllIn(t)
      assertEquals(5, m.end)
    }
    locally {
      val data = "<a>aaaaa</a><b>bbbbbb</b><c>ccccccc</c>"
      val p = "^<a>(.+)</a><b>(.+)</b><c>(.+)</c>$".r
      val parts = p.findAllIn(data)
      val aes = parts.group(1)
      val bes = parts.group(2)
      val ces = parts.group(3)
      assertEquals("ccccccc", ces)
      assertEquals("bbbbbb", bes)
      assertEquals("aaaaa", aes)
    }
  }

  @Test def `t10827 matches method`(): Unit = {
    val r = """\d+""".r
    assertTrue(r.matches("500"))
    assertFalse(r.matches("foo"))
    assertFalse(r.matches("123 123"))
    assertFalse(r.matches("foo2"))
    assertFalse(r.matches("2foo"))
  }

  @Test def `t10827 matches method for unanchored Regex`(): Unit = {
    val r = """\d+""".r.unanchored
    assertTrue(r.matches("500"))
    assertFalse(r.matches("abc"))
    assertTrue(r.matches("123 123"))
    assertTrue(r.matches("foo2"))
    assertTrue(r.matches("2foo"))
  }

  @Test def replacementMatching(): Unit = {
    val regex = """\$\{(.+?)\}""".r
    val replaced = regex.replaceAllIn("Replacing: ${main}. And another method: ${foo}.",
        (m: util.matching.Regex.Match) => {
      val identifier = m.group(1)
      identifier
    })
    assertEquals("Replacing: main. And another method: foo.", replaced)

    val regex3 = """\$\{(.+?)\}""".r
    val replaced3 = regex3.replaceSomeIn("Replacing: ${main}. And another: ${foo}.", (m: util.matching.Regex.Match) => {
      val id = m.group(1)
      if (id.startsWith("m")) Some(id) else None
    })
    assertEquals("Replacing: main. And another: ${foo}.", replaced3)
  }

  @Test def groupsMatching(): Unit = {
    val Date = """(\d+)/(\d+)/(\d+)""".r
    for (Regex.Groups(a, b, c) <- Date findFirstMatchIn "1/1/2001 marks the start of the millennium. 31/12/2000 doesn't.") {
      assertEquals("1", a)
      assertEquals("1", b)
      assertEquals("2001", c)
    }
    for (Regex.Groups(a, b, c) <- Date.findAllIn("1/1/2001 marks the start of the millennium. 31/12/2000 doesn't.").matchData) {
      assertTrue(a == "1" || a == "31")
      assertTrue(b == "1" || b == "12")
      assertTrue(c == "2001" || c == "2000")
    }
  }

  @Test def `t6406 no longer unapply any`(): Unit = {
    val r = "(\\d+)".r
    val q = """(\d)""".r
    val ns = List("1,2","x","3,4")
    val u = r.unanchored

    val is = ns collect { case u(x) => x } map { case r(x) => x }
    assertTrue(List("1", "3").sameElements(is))
    assertEquals(List("1", "3"), is)
    // Match from same pattern
    val js = ns.map(u.findFirstMatchIn(_)).flatten.map { case r(x) => x }
    assertEquals(List("1", "3"), js)
    // Match not from same pattern
    val ks = ns.map(q.findFirstMatchIn(_)).flatten.map { case r(x) => x }
    assertEquals(List("1", "3"), ks)

    val t = "Last modified 2011-07-15"
    val p1 = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
    val y1: Option[String] = for {
      p1(year, month, day) <- p1.findFirstIn(t)
    } yield year
    assertEquals(Some("2011"), y1)
    val y2: Option[String] = for {
      p1(year, month, day) <- p1.findFirstMatchIn(t)
    } yield year
    assertEquals(Some("2011"), y2)
  }
}
