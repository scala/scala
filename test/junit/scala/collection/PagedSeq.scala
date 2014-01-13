package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

/* Test for SI-6615  */
@RunWith(classOf[JUnit4])
class PagedSeqTest {
  @Test
  def rovingDoesNotNPE(): Unit = {
    // should not NPE, and should equal the given Seq
    assertEquals(Seq('a'), PagedSeq.fromStrings(List.fill(5000)("a")).slice(4096, 4097))
  }
}
