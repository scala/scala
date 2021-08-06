package scala.collection

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class LinearSeqOptimizedTest {

  @Test def `t9936 indexWhere`(): Unit = {
    assertEquals(2, "abcde".indexOf('c', -1))
    assertEquals(2, "abcde".indexOf('c', -2))
    assertEquals(2, "abcde".toList.indexOf('c', -1))
    assertEquals(2, "abcde".toList.indexOf('c', -2))
    assertEquals(2, "abcde".toList.indexWhere(_ == 'c', -1))
    assertEquals(2, "abcde".toList.indexWhere(_ == 'c', -2))
  }

  @Test def test_efficientTails_list_SI9892(): Unit = {
    val tails = List(1,2,3,4).tails.toList

    assert(tails(0).tail eq tails(1))
    assert(tails(0).tail.tail eq tails(2))
    assert(tails(1).tail eq tails(2))
    assert(tails(3).tail eq tails(4))
    assert(tails(4) eq List())
  }

  @deprecated("Tests deprecated Stream", since="2.13.0")
  @Test def test_efficientTails_stream_SI9892(): Unit = {
    val stream = Stream.from(1)
    val tails = stream.tails.toStream
    assert(tails.head eq stream)
  }
}
