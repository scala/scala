package strawman.collection

import scala.Predef.{augmentString => _, classOf, assert}
import strawman.collection.immutable.{ List, LazyList }

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

  @Test def test_efficientTails_list_SI9892: Unit = {
    val tails = List(1,2,3,4).tails.toList

    assert(tails(0).tail eq tails(1))
    assert(tails(0).tail.tail eq tails(2))
    assert(tails(1).tail eq tails(2))
    assert(tails(3).tail eq tails(4))
    assert(tails(4) eq List())
  }

  @Test def test_efficientTails_stream_SI9892: Unit = {
    val lazyList = LazyList.from(1)
    val tails = lazyList.tails.to(LazyList)
    assert(tails.head eq lazyList)
  }
}
