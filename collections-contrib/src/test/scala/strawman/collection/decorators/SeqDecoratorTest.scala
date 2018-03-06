package strawman.collection
package decorators

import org.junit.Assert.assertEquals
import org.junit.Test
import strawman.collection.immutable._

import scala.Predef.{intArrayOps => _, genericArrayOps => _, wrapIntArray => _, genericWrapArray => _, $conforms}

class SeqDecoratorTest {

  @Test def intersperse(): Unit = {
    assertEquals(List(1, 0, 2, 0, 3), List(1, 2, 3).intersperse(0))
    assertEquals(List(-1, 1, 0, 2, 0, 3, -2), List(1, 2, 3).intersperse(-1, 0, -2))
    assertEquals(List.empty[Int], List.empty[Int].intersperse(0))
    assertEquals(List(1, 2), List.empty[Int].intersperse(1, 0, 2))
    assertEquals(List(1), List(1).intersperse(0))
    assertEquals(List(0, 1, 2), List(1).intersperse(0, 5, 2))
  }

  // This test just checks that there is no compilation error
  @Test def genericDecorator(): Unit = {
    val vector = Vector(1, 2, 3)
    val range = Range(0, 10)
    val array = Array(1, 2, 3)
    val string = "foo"
    val list = List(1, 2, 3)
    val result = list.intersperse(0)
    typed[List[Int]](result)
    list.view.intersperse(0)
    val result2 = range.intersperse(0)
    typed[IndexedSeq[Int]](result2)
    vector.intersperse(0)
    vector.view.intersperse(0)
    val result3 = array.intersperse(0)
    typed[Array[Int]](result3)
    array.view.intersperse(0)
    string.intersperse(' ')
    string.view.intersperse(' ')
  }

  def typed[T](t: => T): Unit = ()

}
