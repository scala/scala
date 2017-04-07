package scala

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

// these two implicit conversions come from Predef

@RunWith(classOf[JUnit4])
class CharSequenceImplicitsTests {
  @Test def arrayAsCharSequence(): Unit =
    assertEquals("ab", (Array     ('a', 'b'): CharSequence).toString)
  @Test def indexedSeqAsCharSequence(): Unit =
    assertEquals("ab", (IndexedSeq('a', 'b'): CharSequence).toString)
}
