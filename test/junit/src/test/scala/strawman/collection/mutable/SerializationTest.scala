package strawman.collection.mutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.mutable

@RunWith(classOf[JUnit4])
class SerializationTest {

  @Test
  def arrayBuffer(): Unit = {
    assertEqualsAfterDeserialization(mutable.ArrayBuffer.empty[Int])
    assertEqualsAfterDeserialization(mutable.ArrayBuffer(1, 2, 3))
  }

  @Test
  def listBuffer(): Unit = {
    assertEqualsAfterDeserialization(mutable.ListBuffer.empty[Int])
    assertEqualsAfterDeserialization(mutable.ListBuffer(1, 2, 3))
  }

  @Test
  def hashMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.HashMap.empty[Int, String])
    assertEqualsAfterDeserialization(mutable.HashMap(1 -> "one", 2 -> "two", 3 -> "three"))
  }

  @Test
  def linkedHashMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.LinkedHashMap.empty[Int, String])
    assertEqualsAfterDeserialization(mutable.LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three"))
  }

  @Test
  def longMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.LongMap.empty[String])
    assertEqualsAfterDeserialization(mutable.LongMap(1L -> "one", 2L -> "two", 3L -> "three"))
  }

  @Test
  def treeMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.TreeMap.empty[Int, String])
    assertEqualsAfterDeserialization(mutable.TreeMap(1 -> "one", 2 -> "two", 3 -> "three"))
  }

  @Test
  def anyRefMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.AnyRefMap.empty[String, String])
    assertEqualsAfterDeserialization(mutable.AnyRefMap("1" -> "one", "2" -> "two", "3" -> "three"))
  }

  @Test
  def hashSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.HashSet.empty[Int])
    assertEqualsAfterDeserialization(mutable.HashSet(1, 2, 3))
  }

  @Test
  def linkedHashSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.LinkedHashSet.empty[Int])
    assertEqualsAfterDeserialization(mutable.LinkedHashSet(1, 2, 3))
  }

  @Test
  def bitSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.BitSet.empty)
    assertEqualsAfterDeserialization(mutable.BitSet(1, 2, 3))
  }

  @Test
  def treeSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.TreeSet.empty[Int])
    assertEqualsAfterDeserialization(mutable.TreeSet(1, 2, 3))
  }

  private def assertEqualsAfterDeserialization[A](original: mutable.Iterable[A]): Unit = {
    val after = serializeDeserialize(original)
    assertEquals(original, after)
  }

  private def serializeDeserialize[T <: AnyRef](obj: T): T = {
    import java.io._
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}
