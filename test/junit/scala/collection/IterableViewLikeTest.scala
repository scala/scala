package scala.collection

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class IterableViewLikeTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val iter = Iterable(1, 2, 3)

    assertEquals(Iterable.empty[Int], iter.view.take(Int.MinValue).toIndexedSeq)
    assertEquals(Iterable.empty[Int], iter.view.takeRight(Int.MinValue).toIndexedSeq)
    assertEquals(iter, iter.view.drop(Int.MinValue).toIndexedSeq)
    assertEquals(iter, iter.view.dropRight(Int.MinValue).toIndexedSeq)
  }
}
