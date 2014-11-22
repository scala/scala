package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import java.lang.ref._

@RunWith(classOf[JUnit4])
class LinearSeqLikeTest {

  // Wait for the list ref to go stale.
  // A helper thread allocates to goose collection.
  // The allocation delta must be small enough to create
  // pressure without failing outright.
  // In failure mode, the OOME causes the test thread
  // to fail with InterruptedException.
  // The test completes quickly.
  @Test
  def exhaustedIteratorMustNotReferenceUnderlyingSeq(): Unit = {
    //import java.lang.management._
    //Console println ManagementFactory.getMemoryMXBean.getHeapMemoryUsage

    exhaustedIteratorTestFor(List(1, 2, 3), 2)

    exhaustedIteratorTestFor(mutable.MutableList(1, 2, 3), 2)

    //Console println ManagementFactory.getMemoryMXBean.getHeapMemoryUsage
  }

  private def exhaustedIteratorTestFor[A, Repr <: LinearSeqLike[A, Repr]](gen: => Repr, two: A): Unit = {
    val refq = new ReferenceQueue[Repr]

    // a list's iterator and a weak ref to the list
    def data = {
      var list = gen
      val ref  = new WeakReference(list, refq)
      val res  = (ref, list.iterator)
      list     = null.asInstanceOf[Repr]     // paranoia
      res
    }

    val (ref, it) = data

    //assertNotNull(ref.get)                 // likely
    it.next()                                // exercise
    val rest = it.toList                     // must exhaust iterator

    assertTrue(it.isEmpty)
    assertEquals(two, rest.head)

    // apply gentle pressure
    class Alloc(val parent: Thread) extends Runnable {
      @volatile var done  = false
      @volatile var count = 0

      var allocated  = List.empty[Array[Byte]]
      val allocation = 1 * 1024 * 1024

      override def run(): Unit = {
        while (!done) {
          try {
            allocated = Array.ofDim[Byte](allocation) :: allocated
          } catch {
            case _: OutOfMemoryError => parent.interrupt() ; done = true
          }
          count += 1
          Thread.`yield`()
        }
      }
    }
    val alloc   = new Alloc(Thread.currentThread)
    val t       = new Thread(alloc)
    val timeout = 60 * 1000L                 // wait a minute
    val got =
      try {
        t.start()
        refq.remove(timeout)
      } finally alloc.done = true
    assertEquals(ref, got)
    assertNull(ref.get)
    //Console println s"Done after ${alloc.count}"
  }
}
