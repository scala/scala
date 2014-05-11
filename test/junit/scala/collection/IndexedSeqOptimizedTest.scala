package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test

@RunWith(classOf[JUnit4])
class IndexedSeqOptimizedTest {

  @Test
  def notThrowsAnExceptionInLastIndexOf() {
    assertEquals(0, (Array(2): collection.mutable.WrappedArray[Int]).lastIndexWhere(_ => true, 1))
    assertEquals(2, "abc123".lastIndexWhere(_.isLetter, 6))
  }
}
