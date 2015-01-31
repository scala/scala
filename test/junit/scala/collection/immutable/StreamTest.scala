package scala.collection.immutable

import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.ref.WeakReference
import scala.util.Try

@RunWith(classOf[JUnit4])
/* Tests for collection.immutable.Stream  */
class StreamTest {
  val ref = WeakReference( Stream.continually(42).take(100) )

  def gcAndThrowIfCollected(d: Any): Unit = {
    System.gc()
    Thread.sleep(100)
    if (ref.get.isEmpty) throw new RuntimeException("GC succeeded")
  }

  @Test
  def foreachAllowsGC() {
    Try { ref().foreach(gcAndThrowIfCollected) }
    assert( ref.get.isEmpty )
  }

  /** SI-8990: Fix StreamWithFilter#foreach to allow GC of head as tail is processed */
  @Test
  def withFilterForeachAllowsGC() {
    Try { ref().withFilter(_ => true).foreach(gcAndThrowIfCollected) }
    assert( ref.get.isEmpty )
  }

}
