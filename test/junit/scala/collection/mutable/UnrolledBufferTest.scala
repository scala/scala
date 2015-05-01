package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class UnrolledBufferTestTest {
  @Test
  def test_SI9254_original() {
    val b = new UnrolledBuffer[Int]()
    (1 to 16).foreach(i => b append i)
    b.insert(0,-1)
    b append 17
    assert(b sameElements (Seq(-1) ++ (1 to 16) ++ Seq(17)))
  }

  @Test
  def test_SI9254_additional() {
    val b = new UnrolledBuffer[Int]()
    (1 to 100).foreach(i => b append i)
    b.insert(40, -1)
    assert(b sameElements((1 to 40) ++ Seq(-1) ++ (41 to 100)))
  }
}
