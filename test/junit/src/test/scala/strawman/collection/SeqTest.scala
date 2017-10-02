package strawman.collection

import scala.Predef.{augmentString => _, classOf}

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test
import strawman.collection.immutable.{List, Nil, Vector}

@RunWith(classOf[JUnit4])
class SeqTest {

  @Test def `t9936 indexWhere`(): Unit = {
    assertEquals(2, "abcde".indexOf('c', -1))
    assertEquals(2, "abcde".indexOf('c', -2))
    assertEquals(2, "abcde".to(Vector).indexOf('c', -1))
    assertEquals(2, "abcde".to(Vector).indexOf('c', -2))
    assertEquals(2, "abcde".to(Vector).indexWhere(_ == 'c', -1))
    assertEquals(2, "abcde".to(Vector).indexWhere(_ == 'c', -2))
  }

  @Test def combinations(): Unit = {
    assertEquals(List(Nil), Nil.combinations(0).to(List))
    assertEquals(Nil, Nil.combinations(1).to(List))
    assertEquals(List(List(1, 2), List(1, 3), List(2, 3)), List(1, 2, 3).combinations(2).to(List))
    assertEquals(List(List(1, 2, 3)), List(1, 2, 3).combinations(3).to(List))
  }

// Commented because `++:` is no defined on `View`. We might re-enable this test if we introduce a `SeqView`
//  @Test
//  def test_SI8691(): Unit = {
//    // Really just testing to make sure ++: doesn't throw an exception
//    assert( (Seq(1,2) ++: Seq(3,4).view).to(Seq) == Seq(1,2,3,4) )
//  }

  @Test
  def hasCorrectDistinct: Unit = {
    assertEquals(Seq(1, 2, 3, 4, 5), Seq(1, 1, 2, 3, 3, 3, 4, 5, 5).distinct)
  }

  @Test
  def hasCorrectDistinctBy: Unit = {
    val result = Seq("a", "aa", "aaa", "b", "bb", "bbb", "bbbb", "c").distinctBy(_.length)

    assertEquals(Seq("a", "aa", "aaa", "bbbb"), result)
  }
}
