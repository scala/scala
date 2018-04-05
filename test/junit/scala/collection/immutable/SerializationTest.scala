package scala.collection.immutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SerializationTest {

  @Test
  def immutableArray(): Unit = {
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array("1", "2", "3")))
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array(Int.MinValue)))
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array(Long.MinValue)))
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array(Double.MinValue)))
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array(Float.MinValue)))
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array(Char.MinValue)))
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array(Byte.MinValue)))
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array(Short.MinValue)))
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array(true)))
    assertEqualsAfterDeserialization(ImmutableArray.unsafeWrapArray(Array(())))
  }

  @Test
  def hashMap(): Unit = {
    assertEqualsAfterDeserialization(HashMap.empty[Int, String])
    assertEqualsAfterDeserialization(HashMap(1 -> "one", 2 -> "two", 3 -> "three"))
  }

  @Test
  def intMap(): Unit = {
    assertEqualsAfterDeserialization(IntMap.empty[String])
    assertEqualsAfterDeserialization(IntMap(1 -> "one", 2 -> "two", 3 -> "three"))
  }

  @Test
  def longMap(): Unit = {
    assertEqualsAfterDeserialization(LongMap.empty[String])
    assertEqualsAfterDeserialization(LongMap(1L -> "one", 2L -> "two", 3L -> "three"))
  }

  @Test
  def mapWithDefault(): Unit = {
    assertEqualsAfterDeserialization(Map.empty[Int, String].withDefaultValue("none"))
    assertEqualsAfterDeserialization(Map(1 -> "one").withDefaultValue("none"))
  }

  @Test
  def sortedMapWithDefault(): Unit = {
    assertEqualsAfterDeserialization(SortedMap.empty[Int, String].withDefaultValue("none"))
    assertEqualsAfterDeserialization(SortedMap(1 -> "one").withDefaultValue("none"))
  }

  @Test
  def treeMap(): Unit = {
    assertEqualsAfterDeserialization(TreeMap.empty[Int, String])
    assertEqualsAfterDeserialization(TreeMap(1 -> "one", 2 -> "two", 3 -> "three"))
  }

  @Test
  def hashSet(): Unit = {
    assertEqualsAfterDeserialization(HashSet.empty[Int])
    assertEqualsAfterDeserialization(HashSet(1, 2, 3))
  }

  @Test
  def bitSet(): Unit = {
    assertEqualsAfterDeserialization(BitSet.empty)
    assertEqualsAfterDeserialization(BitSet(1, 2, 3))
  }

  @Test
  def treeSet(): Unit = {
    assertEqualsAfterDeserialization(TreeSet.empty[Int])
    assertEqualsAfterDeserialization(TreeSet(1, 2, 3))
  }

//  @Test
//  def lazyList(): Unit = {
//    assertEqualsAfterDeserialization(LazyList.empty)
//    assertEqualsAfterDeserialization(LazyList.from(1))
//  }

  @Test
  def list(): Unit = {
    assertEqualsAfterDeserialization(Nil)
    assertEqualsAfterDeserialization(List(1, 2, 3))
  }

  @Test
  def listMap(): Unit = {
    assertEqualsAfterDeserialization(ListMap.empty[Int, String])
    assertEqualsAfterDeserialization(ListMap(1 -> "one", 2 -> "two", 3 -> "three"))
  }

  @Test
  def listSet(): Unit = {
    assertEqualsAfterDeserialization(ListSet.empty[Int])
    assertEqualsAfterDeserialization(ListSet(1, 2, 3))
  }

  @Test
  def numericRange(): Unit = {
    assertEqualsAfterDeserialization(NumericRange(start = 0, end = 10, step = 1))
  }

  @Test
  def range(): Unit = {
    assertEqualsAfterDeserialization(Range(start = 0, end = 10, step = 1))
  }

  @Test
  def vector(): Unit = {
    assertEqualsAfterDeserialization(Vector.empty[Int])
    assertEqualsAfterDeserialization(Vector(1, 2, 3))
  }

  private def assertEqualsAfterDeserialization[A](original: Iterable[A]): Unit = {
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
