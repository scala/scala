package strawman.collection
package decorators

import org.junit.Assert.assertEquals
import org.junit.Test

import strawman.collection.immutable.List

class SeqDecoratorTest {

  @Test def intersperse(): Unit = {
    assertEquals(List(1, 0, 2, 0, 3), List(1, 2, 3).intersperse(0))
    assertEquals(List(-1, 1, 0, 2, 0, 3, -2), List(1, 2, 3).intersperse(-1, 0, -2))
    assertEquals(List.empty[Int], List.empty[Int].intersperse(0))
    assertEquals(List(1, 2), List.empty[Int].intersperse(1, 0, 2))
    assertEquals(List(1), List(1).intersperse(0))
    assertEquals(List(0, 1, 2), List(1).intersperse(0, 5, 2))
  }

}
