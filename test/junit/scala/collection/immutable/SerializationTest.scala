package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SerializationTest {

  @Test
  def arraySeq(): Unit = {
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array("1", "2", "3")))
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array(Int.MinValue)))
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array(Long.MinValue)))
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array(Double.MinValue)))
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array(Float.MinValue)))
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array(Char.MinValue)))
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array(Byte.MinValue)))
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array(Short.MinValue)))
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array(true)))
    assertEqualsAfterDeserialization(ArraySeq.unsafeWrapArray(Array(())))
  }

  @Test
  def hashMap(): Unit = {
    assertEqualsAfterDeserialization(HashMap.empty[Int, String], classOf[HashMap[_, _]])
    assertEqualsAfterDeserialization(HashMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[HashMap[_, _]])
    assertEquals(HashMap(1 -> 1, 2 -> 2, 3 -> 3), serializeDeserialize(HashMap(1 -> "1", 2 -> "2", 3 -> "3").view.mapValues(_.toInt)).toMap)
    assertEquals(HashMap(2 -> "2", 3 -> "3"), serializeDeserialize(HashMap(1 -> "1", 2 -> "2", 3 -> "3").view.filterKeys(_ > 1)).toMap)
  }

  @Test
  def intMap(): Unit = {
    assertEqualsAfterDeserialization(IntMap.empty[String], classOf[IntMap[_]])
    assertEqualsAfterDeserialization(IntMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[IntMap[_]])
  }

  @Test
  def longMap(): Unit = {
    assertEqualsAfterDeserialization(LongMap.empty[String], classOf[LongMap[_]])
    assertEqualsAfterDeserialization(LongMap(1L -> "one", 2L -> "two", 3L -> "three"), classOf[LongMap[_]])
  }

  @Test
  def mapWithDefault(): Unit = {
    assertEqualsAfterDeserialization(Map.empty[Int, String].withDefaultValue("none"), classOf[Map[_, _]])
    assertEqualsAfterDeserialization(Map(1 -> "one").withDefaultValue("none"), classOf[Map[_, _]])
  }

  @Test
  def sortedMapWithDefault(): Unit = {
    assertEqualsAfterDeserialization(SortedMap.empty[Int, String].withDefaultValue("none"), classOf[SortedMap[_, _]])
    assertEqualsAfterDeserialization(SortedMap(1 -> "one").withDefaultValue("none"), classOf[SortedMap[_, _]])
  }

  @Test
  def treeMap(): Unit = {
    assertEqualsAfterDeserialization(TreeMap.empty[Int, String], classOf[TreeMap[_, _]])
    assertEqualsAfterDeserialization(TreeMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[TreeMap[_, _]])
  }

  @Test
  def hashSet(): Unit = {
    assertEqualsAfterDeserialization(HashSet.empty[Int], classOf[HashSet[_]])
    assertEqualsAfterDeserialization(HashSet(1, 2, 3), classOf[HashSet[_]])
  }

  @Test
  def bitSet(): Unit = {
    assertEqualsAfterDeserialization(BitSet.empty, classOf[BitSet])
    assertEqualsAfterDeserialization(BitSet(1, 2, 3), classOf[BitSet])
  }

  @Test
  def treeSet(): Unit = {
    assertEqualsAfterDeserialization(TreeSet.empty[Int], classOf[TreeSet[_]])
    assertEqualsAfterDeserialization(TreeSet(1, 2, 3), classOf[TreeSet[_]])
  }

  @Test
  def lazyList(): Unit = {
    assertEqualsAfterDeserialization(LazyList.empty, classOf[LazyList[_]])
    val l = serializeDeserialize(LazyList.from(1))
    assertEquals(1 to 5, l.take(5))
    assertEqualsAfterDeserialization(LazyList.from(1).take(10000).force, classOf[LazyList[_]])
  }

  @Test
  def stream(): Unit = {
    assertEqualsAfterDeserialization(Stream.empty, classOf[Stream[_]])
    val s = serializeDeserialize(Stream.from(1))
    assertEquals(1 to 5, s.take(5))
    assertEqualsAfterDeserialization(Stream.from(1).take(10000).force, classOf[Stream[_]])
  }

  @Test
  def list(): Unit = {
    assertEqualsAfterDeserialization(Nil, Nil.getClass)
    assertEqualsAfterDeserialization(List(1, 2, 3), classOf[List[_]])
  }

  @Test
  def listMap(): Unit = {
    assertEqualsAfterDeserialization(ListMap.empty[Int, String], classOf[ListMap[_, _]])
    assertEqualsAfterDeserialization(ListMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[ListMap[_, _]])
  }

  @Test
  def listSet(): Unit = {
    assertEqualsAfterDeserialization(ListSet.empty[Int], classOf[ListSet[_]])
    assertEqualsAfterDeserialization(ListSet(1, 2, 3), classOf[ListSet[_]])
  }

  @Test
  def numericRange(): Unit = {
    assertEqualsAfterDeserialization(NumericRange(start = 0, end = 10, step = 1), classOf[NumericRange[_]])
  }

  @Test
  def range(): Unit = {
    assertEqualsAfterDeserialization(Range(start = 0, end = 10, step = 1), classOf[Range])
  }

  @Test
  def vector(): Unit = {
    assertEqualsAfterDeserialization(Vector.empty[Int], classOf[Vector[_]])
    assertEqualsAfterDeserialization(Vector(1, 2, 3), classOf[Vector[_]])
  }

  private def assertEqualsAfterDeserialization[A](original: Iterable[A], expectedClass: Class[_] = null): Unit = {
    val after = serializeDeserialize(original)
    assertEquals(original, after)
    if(expectedClass ne null)
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
