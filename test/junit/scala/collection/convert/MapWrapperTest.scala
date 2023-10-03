package scala.collection.convert

import java.{util => jutil}

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.jdk.CollectionConverters._
import scala.tools.testkit.AssertUtil.{assertSucceeds, assertThrows}
import scala.util.chaining._

@RunWith(classOf[JUnit4])
class MapWrapperTest {

  /* Test for scala/bug#7883 */
  @deprecated("Uses deprecated extension", since="2.13")
  @Test
  def testContains(): Unit = {
    import scala.language.reflectiveCalls  // for accessing containsCounter

    // A HashMap which throws an exception when the iterator method is called.
    // Before the fix for scala/bug#7883, calling MapWrapper.containsKey() used to
    // iterate through every element of the wrapped Map, and thus would crash
    // in this case.
    val scalaMap = new scala.collection.mutable.HashMap[String, String] {
      var containsCounter = 0  // keep track of how often contains() has been called.
      override def iterator = throw new UnsupportedOperationException

      override def contains(key: String): Boolean = {
        containsCounter += 1
        super.contains(key)
      }
    }

    val javaMap = scalaMap.asJava

    scalaMap("hello") = "world"
    scalaMap(null) = "null's value"

    assertEquals(0, scalaMap.containsCounter)
    assertTrue(javaMap.containsKey("hello"))     // positive test
    assertTrue(javaMap.containsKey(null))        // positive test, null key

    assertFalse(javaMap.containsKey("goodbye"))  // negative test
    // Note: this case does NOT make it to scalaMap's contains() method because the runtime
    // cast fails in MapWrapper, so the containsCounter is not incremented in this case.
    assertFalse(javaMap.containsKey(42))         // negative test, wrong key type

    assertEquals(Some("null's value"), scalaMap.remove(null))
    assertFalse(javaMap.containsKey(null))       // negative test, null key
    assertEquals(4, scalaMap.containsCounter)
  }

  // test for scala/bug#8504
  @Test
  def testHashCodeNulls(): Unit = {
    val javaMap = Map(1 -> null).asJava

    // Before the fix for scala/bug#8504, this throws a NPE
    assertSucceeds(javaMap.hashCode)
  }

  // regression test for https://github.com/scala/bug/issues/10663
  @Test
  def testHashCodeEqualsMatchesJavaMap(): Unit = {
    val jmap = new jutil.HashMap[String, String]()
    jmap.put("scala", "rocks")
    jmap.put("java interop is fun!", "ya!")
    jmap.put("Ĺởồҝ ïŧ\\'ş ūŋǐčōđẹ", "whyyyy")
    jmap.put("nulls nooo", null)
    jmap.put(null, "null keys are you serious??")

    // manually convert to scala map
    val scalaMap = jmap.entrySet().iterator.asScala.map { e => e.getKey -> e.getValue}.toMap

    val mapWrapper = scalaMap.asJava

    assertEquals(jmap.hashCode(), mapWrapper.hashCode())
    assertTrue(jmap == mapWrapper)
    assertTrue(mapWrapper == jmap)
  }

  // was: induce intermittent failure due to contention, where updater is called more than once
  @Test def `t12586 updateWith should delegate to compute`: Unit = {
    val limit = 100      // retries until trigger
    @volatile var count = 0
    val jmap = new jutil.concurrent.ConcurrentHashMap[String, String]()
    class Loki extends Runnable {
      @volatile var done = false
      def run(): Unit = {
        while (!done) {
          jmap.put("KEY", "VALUE")
          //Thread.`yield`()
        }
      }
    }
    val loki = new Loki
    val runner = new Thread(loki).tap(_.start)
    val wrapped = jmap.asScala
    def updater(old: Option[String]) = { count += 1 ; old.map(_ * 2) }
    for (i <- 1 to limit) {
      count = 0
      wrapped.updateWith("KEY")(updater)
      assertEquals(s"index $i", 1, count)
    }
    loki.done = true
    runner.join()
  }
  @Test def `updateWith and getOrElseUpdate should reflect null policy of update`: Unit = {
    val jmap = new jutil.concurrent.ConcurrentHashMap[String, String]()
    val wrapped = jmap.asScala
    assertThrows[NullPointerException](jmap.put("K", null))
    assertThrows[NullPointerException](jmap.putIfAbsent("K", null))
    assertThrows[NullPointerException](wrapped.put("K", null))
    assertThrows[NullPointerException](wrapped.update("K", null))
    assertThrows[NullPointerException](wrapped.updateWith("K")(_ => Some(null)))
    assertThrows[NullPointerException](wrapped.getOrElseUpdate("K", null))

    var count = 0
    def v = {
      count += 1
      null
    }
    assertThrows[NullPointerException](wrapped.update("K", v))
    assertEquals(1, count)
    assertThrows[NullPointerException](wrapped.updateWith("K")(_ => Some(v)))
    assertEquals(3, count)  // extra count in retry
  }
  @Test def `more updateWith and getOrElseUpdate should reflect null policy of update`: Unit = {
    val jmap = new jutil.HashMap[String, String]()
    val wrapped = jmap.asScala
    wrapped.put("K", null)
    assertEquals(1, wrapped.size)
    wrapped.remove("K")
    assertEquals(0, wrapped.size)
    wrapped.update("K", null)
    assertEquals(1, wrapped.size)
    wrapped.remove("K")
    wrapped.updateWith("K")(_ => Some(null))
    assertEquals(1, wrapped.size)
    wrapped.remove("K")
    wrapped.getOrElseUpdate("K", null)
    assertEquals(1, wrapped.size)

    var count = 0
    def v = {
      count += 1
      null
    }
    wrapped.update("K", v)
    assertEquals(1, count)
    wrapped.remove("K")
    wrapped.updateWith("K")(_ => Some(v))
    assertEquals(2, count)
  }

  @Test def `getOrElseUpdate / updateWith support should insert null`: Unit = {
    val jmap = new jutil.HashMap[String, String]()
    val wrapped = jmap.asScala

    wrapped.getOrElseUpdate("a", null)
    assertTrue(jmap.containsKey("a"))

    wrapped.getOrElseUpdate(null, "x")
    assertTrue(jmap.containsKey(null))

    jmap.clear()

    wrapped.updateWith("b")(_ => Some(null))
    assertTrue(jmap.containsKey("b"))

    wrapped.updateWith(null)(_ => Some("x"))
    assertTrue(jmap.containsKey(null))
  }
}
