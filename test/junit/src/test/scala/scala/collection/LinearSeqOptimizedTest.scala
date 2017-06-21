package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test

@RunWith(classOf[JUnit4])
class LinearSeqOptimizedTest {

  @Test def `t9936 indexWhere`(): Unit = {
    assertEquals(2, "abcde".indexOf('c', -1))
    assertEquals(2, "abcde".indexOf('c', -2))
    assertEquals(2, "abcde".toList.indexOf('c', -1))
    assertEquals(2, "abcde".toList.indexOf('c', -2))
    assertEquals(2, "abcde".toList.indexWhere(_ == 'c', -1))
    assertEquals(2, "abcde".toList.indexWhere(_ == 'c', -2))
  }
}
