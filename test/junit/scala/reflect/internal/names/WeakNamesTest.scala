package scala.reflect.internal.names

import java.util

import org.junit._
import Assert._

import java.lang.ref.WeakReference

trait WeakNamesTest {
  self: ExtendedNameTest =>

  val weakSources = Vector.tabulate(100)(i => s"weak${i}")

  private def gcHard(): Unit = {
    for (i <- 1 to 3) {
      System.gc()
      System.runFinalization()
      Thread.sleep(1)
    }
  }
  private def gcHard(completeWhenClear: WeakReference[T]): Unit = {
    for (i <- 1 to 10 if (completeWhenClear.get ne null)) {
      System.gc()
      System.runFinalization()
      Thread.sleep(1)
    }
  }

  @Test def simpleWeak: Unit = {
    val size = weakSources.length
    val strongRefs = new Array[AnyRef](size)
    for (i <- 0 until size) {
      val name = newTermName(weakSources(i))
      assertNotNull(name)
      strongRefs(i) = name
    }
    assertEquals(size, nameTable.size)
    gcHard
    assertEquals(size, nameTable.size)
    gcHard

    util.Arrays.fill(strongRefs, null)
    assertEquals(size, nameTable.size)
  }

  // for colletction that need explicit trimming after GC, trimm them
  def cleanupIfNeeded() = {}

  @Test def slidingWeak: Unit = {
    val size = weakSources.length
    val weakRefs = new Array[WeakReference[T]](size)
    val strongRefs = new Array[AnyRef](size)
    val lag = 10
    for (i <- 0 until lag) {
      val name = newTermName(weakSources(i))
      assertNotNull(name)
      weakRefs(i) = new WeakReference(name)
      strongRefs(i) = name
    }
    assertEquals(lag, nameTable.size)
    gcHard()
    assertEquals(lag, nameTable.size)
    for (tail <- 0 until size) {
      val head = tail + lag
      if (head < size) {
        val name = newTermName(weakSources(head))
        assertNotNull(name)
        weakRefs(head) = new WeakReference(name)
        strongRefs(head) = name
        assertEquals(lag + 1, nameTable.size)

        gcHard()
        assertEquals(lag + 1, nameTable.size)
        assertNotNull(weakRefs(tail).get)
        assertEquals(lag + 1, nameTable.size)
      }
      assertNotNull(strongRefs(tail))
      strongRefs(tail) = null
      gcHard(weakRefs(tail))
      assertNull(s"still a reference to ${weakSources(tail)}", weakRefs(tail).get)
      cleanupIfNeeded()
      assertEquals(s"at tail $tail, head $head, size = ${nameTable.size}", math.min(head, size - 1) - tail, nameTable.size)
    }
    assertEquals(0, nameTable.size)
  }

}
