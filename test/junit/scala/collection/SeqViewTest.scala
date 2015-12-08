package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test

@RunWith(classOf[JUnit4])
class SeqViewTest {

  @Test
  def test_SI8691() {
    // Really just testing to make sure ++: doesn't throw an exception
    assert( Seq(1,2) ++: Seq(3,4).view == Seq(1,2,3,4) )
  }
}
