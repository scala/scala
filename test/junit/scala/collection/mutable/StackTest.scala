package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.IterableFactory

@RunWith(classOf[JUnit4])
class StackTest {

  @Test
  def reversingReturnsAStack(): Unit = {
    val s1 = Stack(1, 2, 3)
    val s2: Stack[Int] = s1.reverse
    assertEquals("Stack", s2.className)
  }
}
