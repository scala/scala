package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StackTest {

  @Test
  def reversingReturnsAStack(): Unit = {
    val s1 = Stack(1, 2, 3)
    val s2: Stack[Int] = s1.reverse
    assertEquals("Stack", s2.collectionClassName)
  }

  @Test
  def insertsWhenResizeIsNeeded(): Unit = {
    val stack = Stack.from(Array.range(0, 15))
    stack.insert(1, -1)
    assertEquals(Stack(0, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), stack)
  }

  @Test
  def sliding(): Unit = ArrayDequeTest.genericSlidingTest(Stack, "Stack")
}
