package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StreamTest {

  @Test(timeout=10000)
  def filterNotWorks() {
    assertEquals(Seq(6, 7, 8), Stream.from(1).filterNot(_ <= 5).take(3))
    assertEquals(Seq((), ()), Stream.continually(()).filterNot(_ => false).take(2))
  }
}
