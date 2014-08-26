package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class StreamTest {

  @Test
  def t6727_and_t6440(): Unit = {
    assertTrue(Stream.continually(()).filter(_ => true).take(2) == Seq((), ()))
    assertTrue(Stream.continually(()).filterNot(_ => false).take(2) == Seq((), ()))
    assertTrue(Stream(1,2,3,4,5).filter(_ < 4) == Seq(1,2,3))
    assertTrue(Stream(1,2,3,4,5).filterNot(_ > 4) == Seq(1,2,3,4))
  }
}
