/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.testkit

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

import java.lang.ref._
import scala.annotation.unused
import scala.tools.testkit.AssertUtil._

class AssertUtilTest {
  @Test def assertThrowsAssertion(): Unit = {
    assertThrows[AssertionError](throw new AssertionError("meme"), _ == "meme")
    try {
      assertThrows[AssertionError](())
      assert(false, "should have thrown!")
    } catch {
      case e: AssertionError if e.getMessage == "Expression did not throw!" =>
    }
  }

  @Test def reachableIgnoresReferences(): Unit = {
    class Holder[A](val ref: SoftReference[A])
    val o = new Object
    val r = new SoftReference(o)
    assertNotReachable(o, new Holder(r)) { }
  }

  @Test def reachableFollowArrays(): Unit = {
    class Holder[A](val ref: SoftReference[A])
    val o = new Object
    val r = new SoftReference(o)
    assertNotReachable(o, Array(new Holder(r))) { }
    assertNotReachable(o, Array(Array(r))) { }
    assertThrows[AssertionError](assertNotReachable(o, Array(Array(o))) { })
    assertThrows[AssertionError](assertNotReachable(o, new Object { @unused val f = Array(o) }) { })
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

  /** TODO
  @Test def `hexdump is supplementary-aware`: Unit = {
    assertEquals("00000000  f0 90 90 80                                       |𐐀.|", hexdump("\ud801\udc00").next())
  }
  */
}
