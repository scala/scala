package scala.tools.testkit

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import AssertUtil._

import java.lang.ref._

@RunWith(classOf[JUnit4])
class AssertUtilTest {

  @Test def reachableIgnoresReferences(): Unit = {
    class Holder[A](val ref: SoftReference[A])
    val o = new Object
    val r = new SoftReference(o)
    assertNotReachable(o, new Holder(r)) { }
  }

  @Test def `asserts on child threads are suppressed`(): Unit = {
    def kickoff(body: => Unit): Unit = {
      val t = new Thread(() => body)
      t.start()
      t.join()
    }
    val sut = withoutATrace {
      kickoff(assertEquals(42, 17))
      kickoff(???)
      kickoff(assertEquals("hi", "bi"))
      kickoff(assertEquals("low", "brow"))
    }
    sut.run()
    sut.asserted match {
      case None    => fail("Expected assertion errors")
      case Some(e) => assertEquals(2, e.getSuppressed.length)
    }
  }

  @Test def `waits for child threads to complete`(): Unit = {
    import java.util.concurrent.CountDownLatch
    val latch = new CountDownLatch(1)
    def kickoff(body: => Unit): Unit = {
      val t = new Thread(() => body)
      t.start()
    }
    val sut = withoutATrace {
      kickoff {
        latch.await()
        assertEquals(42, 17)  // must wait to see this
      }
      kickoff {
        Thread.sleep(100L)    // make them wait for it
        latch.countDown()
      }
      kickoff(assertEquals("hi", "bi"))    // ordinary background thread assertion
      assertEquals("low", "brow")          // "foreground" thread assertion must be handled
    }
    sut.run()
    sut.asserted match {
      case None    => fail("Expected assertion errors")
      case Some(e) => assertEquals(2, e.getSuppressed.length)
    }
  }

  @Test def `result is returned`(): Unit = {
    def kickoff(body: => Unit): Unit = new Thread(() => body).start()
    def f() = {
      kickoff {
        assertEquals(42, 17)
      }
      27
    }
    val sut = withoutATrace(f())
    sut.run()
    assertEquals(Some(27), sut.result)
    assertEquals(1, sut.errors.size)
    assertEquals(0, sut.errors.head._2.getSuppressed.length)
  }
}
