package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.mutable

@RunWith(classOf[JUnit4])
@annotation.nowarn("cat=deprecation&origin=scala.collection.mutable.AnyRefMap")
class SerializationTest {

  @Test
  def arrayBuffer(): Unit = {
    assertEqualsAfterDeserialization(mutable.ArrayBuffer.empty[Int], classOf[mutable.ArrayBuffer[_]])
    assertEqualsAfterDeserialization(mutable.ArrayBuffer(1, 2, 3), classOf[mutable.ArrayBuffer[_]])
  }

  @Test
  def listBuffer(): Unit = {
    assertEqualsAfterDeserialization(mutable.ListBuffer.empty[Int], classOf[mutable.ListBuffer[_]])
    assertEqualsAfterDeserialization(mutable.ListBuffer(1, 2, 3), classOf[mutable.ListBuffer[_]])
  }

  @Test
  def hashMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.HashMap.empty[Int, String], classOf[mutable.HashMap[_, _]])
    assertEqualsAfterDeserialization(mutable.HashMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[mutable.HashMap[_, _]])
  }

  @Test
  def linkedHashMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.LinkedHashMap.empty[Int, String], classOf[mutable.LinkedHashMap[_, _]])
    assertEqualsAfterDeserialization(mutable.LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[mutable.LinkedHashMap[_, _]])
  }

  @Test
  def longMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.LongMap.empty[String], classOf[mutable.LongMap[_]])
    assertEqualsAfterDeserialization(mutable.LongMap(1L -> "one", 2L -> "two", 3L -> "three"), classOf[mutable.LongMap[_]])
  }

  @Test
  def treeMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.TreeMap.empty[Int, String], classOf[mutable.TreeMap[_, _]])
    assertEqualsAfterDeserialization(mutable.TreeMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[mutable.TreeMap[_, _]])
  }

  @Test
  def anyRefMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.AnyRefMap.empty[String, String], classOf[mutable.AnyRefMap[_, _]])
    assertEqualsAfterDeserialization(mutable.AnyRefMap("1" -> "one", "2" -> "two", "3" -> "three"), classOf[mutable.AnyRefMap[_, _]])
  }

  @Test
  def mapWithDefault(): Unit = {
    assertEqualsAfterDeserialization(mutable.Map.empty[Int, String].withDefaultValue("none"), classOf[mutable.Map[_, _]])
    assertEqualsAfterDeserialization(mutable.Map(1 -> "one").withDefaultValue("none"), classOf[mutable.Map[_, _]])
  }

  @Test
  def sortedMapWithDefault(): Unit = {
    assertEqualsAfterDeserialization(mutable.SortedMap.empty[Int, String].withDefaultValue("none"), classOf[mutable.SortedMap[_, _]])
    assertEqualsAfterDeserialization(mutable.SortedMap(1 -> "one").withDefaultValue("none"), classOf[mutable.SortedMap[_, _]])
  }

  @Test
  def hashSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.HashSet.empty[Int], classOf[mutable.HashSet[_]])
    assertEqualsAfterDeserialization(mutable.HashSet(1, 2, 3), classOf[mutable.HashSet[_]])
  }

  @Test
  def linkedHashSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.LinkedHashSet.empty[Int], classOf[mutable.LinkedHashSet[_]])
    assertEqualsAfterDeserialization(mutable.LinkedHashSet(1, 2, 3), classOf[mutable.LinkedHashSet[_]])
  }

  @Test
  def bitSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.BitSet.empty, classOf[mutable.BitSet])
    assertEqualsAfterDeserialization(mutable.BitSet(1, 2, 3), classOf[mutable.BitSet])
  }

  @Test
  def treeSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.TreeSet.empty[Int], classOf[mutable.TreeSet[_]])
    assertEqualsAfterDeserialization(mutable.TreeSet(1, 2, 3), classOf[mutable.TreeSet[_]])
  }

  @Test
  def priorityQueue(): Unit = {
    assertEquals(Seq(), serializeDeserialize(mutable.PriorityQueue.empty[Int]).toSeq)
    assertEquals(Seq(3, 2, 1), serializeDeserialize(mutable.PriorityQueue(1, 2, 3)).toSeq)
  }

  @Test
  def queue(): Unit = {
    assertEquals(Seq(), serializeDeserialize(mutable.Queue.empty[Int]).toSeq)
    assertEquals(Seq(1, 2, 3), serializeDeserialize(mutable.Queue(1, 2, 3)).toSeq)
  }

  private def assertEqualsAfterDeserialization[A](original: mutable.Iterable[A], expectedClass: Class[_]): Unit = {
    val after = serializeDeserialize(original)
    assertEquals(original, after)
    assertTrue("Deserialized class "+after.getClass.getName+" is not assignable to "+expectedClass.getName, expectedClass.isInstance(after))
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
