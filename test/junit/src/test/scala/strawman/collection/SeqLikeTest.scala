package strawman.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test

@RunWith(classOf[JUnit4])
class SeqLikeTest {

  @Test def `t9936 indexWhere`(): Unit = {
    assertEquals(2, "abcde".indexOf('c', -1))
    assertEquals(2, "abcde".indexOf('c', -2))
    assertEquals(2, "abcde".toVector.indexOf('c', -1))
    assertEquals(2, "abcde".toVector.indexOf('c', -2))
    assertEquals(2, "abcde".toVector.indexWhere(_ == 'c', -1))
    assertEquals(2, "abcde".toVector.indexWhere(_ == 'c', -2))
  }

  @Test def combinations(): Unit = {
    assertEquals(List(Nil), Nil.combinations(0).toList)
    assertEquals(Nil, Nil.combinations(1).toList)
    assertEquals(List(List(1, 2), List(1, 3), List(2, 3)), List(1, 2, 3).combinations(2).toList)
    assertEquals(List(List(1, 2, 3)), List(1, 2, 3).combinations(3).toList)
  }
}
