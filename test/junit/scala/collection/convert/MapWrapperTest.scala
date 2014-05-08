package scala.collection.convert

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MapWrapperTest {

  /* Test for SI-7883 */
  @Test
  def testContains() {
    import scala.collection.JavaConverters.mapAsJavaMapConverter
    import scala.language.reflectiveCalls  // for accessing containsCounter

    // A HashMap which throws an exception when the iterator() method is called.
    // Before the fix for SI-7883, calling MapWrapper.containsKey() used to
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

  // test for SI-8504
  @Test
  def testHashCode() {
    import scala.collection.JavaConverters._
    val javaMap = Map(1 -> null).asJava

    // Before the fix for SI-8504, this throws a NPE
    javaMap.hashCode
  }
}
