package strawman.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test

@RunWith(classOf[JUnit4])
class SeqTest {

  @Test
  def test_SI8691(): Unit = {
    // Really just testing to make sure ++: doesn't throw an exception
    assert( (Seq(1,2) ++: Seq(3,4).view).to(Seq) == Seq(1,2,3,4) )
  }

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
