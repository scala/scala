package strawman.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class MutableListTest {
  
  // Tests scala/bug#8976
  @Test def tailIteratorMustTerminateAtLength(): Unit = {
    val is = MutableList(1,2,3)
    val tl = is.tail
    assertEquals(tl.length, tl.iterator().length)
    is += 5
    assertEquals(tl.length, tl.iterator().length)
    assertTrue(tl.sameElements(tl.iterator()))
  }
  @Test def iteratorMustFailEventually(): Unit = assertThrows[NoSuchElementException] {
    MutableList[Unit]().iterator().next()
  }
  // was: Root empty iterator held reference
  @Test def iteratorMustNotHoldOntoLast(): Unit = {
    val is = MutableList(Some(1), Some(2))
    val it = is.iterator()
    val x  = Some(3)
    is += x
    assertNotReachable(x, it) {
      it.next()
      it.next()
    }
    assertTrue(it.isEmpty)
  }
}
