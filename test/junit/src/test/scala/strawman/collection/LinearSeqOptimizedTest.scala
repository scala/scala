package strawman.collection

import scala.Predef.{augmentString => _, classOf}
import strawman.collection.immutable.List

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test

@RunWith(classOf[JUnit4])
class LinearSeqOptimizedTest {

  @Test def `t9936 indexWhere`(): Unit = {
    assertEquals(2, "abcde".indexOf('c', -1))
    assertEquals(2, "abcde".indexOf('c', -2))
    assertEquals(2, "abcde".to(List).indexOf('c', -1))
    assertEquals(2, "abcde".to(List).indexOf('c', -2))
    assertEquals(2, "abcde".to(List).indexWhere(_ == 'c', -1))
    assertEquals(2, "abcde".to(List).indexWhere(_ == 'c', -2))
  }
}
