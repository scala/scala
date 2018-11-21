package scala.collection

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class LinearSeqTest {
  // Tests regression on issue 11262
  @Test def extensionIteratorTest: Unit = {
    class ConstantLinearSeq[A](len: Int, elt: A) extends scala.collection.LinearSeq[A] {
      override val isEmpty: Boolean = len == 0
      override val head = elt
      override lazy val tail = if(len > 0) new ConstantLinearSeq(len - 1, elt) else throw new Exception("tail of empty Seq")
    }

    val x = new ConstantLinearSeq(4, 7)

    val it = x.iterator // The main thing we want to test is that this does not throw an exception
    assert(it.hasNext == true) // Call it at least once so that it won't be optimized away
  }
}
