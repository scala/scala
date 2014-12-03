package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import java.lang.ref._
import java.lang.management._
import java.lang.Thread.{ `yield` => share }

@RunWith(classOf[JUnit4])
class LinearSeqLikeTest {

  /** Exhaust iterator and wait for ref to underlying list to go stale.
   *
   *  A helper thread allocates to goose collection.
   *  The allocation delta must be small enough to create
   *  pressure without failing outright.
   *
   *  In failure mode, the heap bump (or OOME) causes the test thread
   *  to fail with InterruptedException.
   *
   *  To avoid false negatives on heap bump, the reference queue is checked
   *  a second time, so in that case the timeout period is always incurred.
   *  The long timeout accommodates loaded test servers.
   *
   *  Otherwise, the test completes quickly.
   */
  @Test
  def exhaustedIteratorMustNotReferenceUnderlyingSeq(): Unit = {
    exhaustedIteratorTestFor(List(1, 2, 3), 2)
    exhaustedIteratorTestFor(mutable.MutableList(1, 2, 3), 2)
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

    assertTrue(it.isEmpty)                   // exhausted
    assertEquals(two, rest.head)             // sanity check

    val alloc   = new Alloc(Thread.currentThread)
    val t       = new Thread(alloc)
    val timeout = 60 * 1000L                 // wait a minute

    // in case alloc failed spuriously (maybe due to other activity in the JVM)
    // give gc another chance to do the right thing
    def retry() = {
      assertTrue(alloc.fault)
      val retry = refq.remove(timeout)
      if (retry == null) fail(s"Memory exhaustion (${alloc.cause})")
      else retry
    }
    val got =
      try {
        t.start()
        refq.remove(timeout)
      } catch {
        case _: InterruptedException if alloc.fault => retry()
        case e: InterruptedException                => throw e
      } finally {
        try {
          alloc.done = true
          t.join(timeout)
        } catch {
          // ignore error after refq.remove completes
          case e: InterruptedException => if (!alloc.fault) throw e
        }
      }
    assertEquals("Timeout remove", ref, got) // null on timeout
    assertNull("Not collectable", ref.get)   // collection was collectable
    //Console println s"Done after ${alloc.count}"
  }

  // apply gentle pressure
  private class Alloc(val parent: Thread) extends Runnable {
    @volatile var done  = false
    @volatile var fault = false
    @volatile var cause = "NONE"
    @volatile var count = 0

    var allocated  = Array.empty[Byte]
    val allocation = 1 * 1024 * 1024

    // allocate until heap grows or OOM, then interrupt parent.
    override def run(): Unit = {
      val mxb = ManagementFactory.getMemoryMXBean
      val mem = mxb.getHeapMemoryUsage
      //Console println mem
      def fail(what: String) =
        if (!done) {
          done  = true
          fault = true
          cause = what
          parent.interrupt()
          //Console println mxb.getHeapMemoryUsage
        }
      while (!done) {
        try {
          allocated = Array.ofDim[Byte](allocation)
          count += 1
          if (mxb.getHeapMemoryUsage.getCommitted > mem.getCommitted) fail("BUMP")
        } catch {
          case _: OutOfMemoryError => fail("OOME")
        }
        assertEquals(allocation, allocated.length)
        share()                              // share the CPU
      }
    }
  }
}
