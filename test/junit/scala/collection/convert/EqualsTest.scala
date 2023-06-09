
package scala.collection.convert

import org.junit.Test
import org.junit.Assert._

import java.util.{
  AbstractList,
  AbstractMap,
  AbstractSet,
  Collections,
  Collection => JCollection,
  HashSet => JHashSet,
  List => JList,
  Map => JMap,
  Set => JSet
}
import java.lang.{Iterable => JIterable}
import java.util.concurrent.{ConcurrentHashMap => JCMap}
import scala.collection.{AbstractIterable, concurrent, mutable}
import scala.jdk.CollectionConverters._
import JavaCollectionWrappers._

class JTestList(vs: Int*) extends AbstractList[Int] {
  def this() = this(Nil: _*)
  override def size = vs.size
  override def get(i: Int) = vs(i)
}
class JTestSet(vs: Int*) extends AbstractSet[Int] {
  def this() = this(Nil: _*)
  require(vs.toSet.size == vs.size)
  override def size = vs.size
  override def iterator = vs.iterator.asJava
}

object JTestMap {
  case class JTestMapEntry(key: Int, value: String) extends JMap.Entry[Int, String] {
    override def getKey: Int = key
    override def getValue: String = value
    override def setValue(value: String): String =
      throw new UnsupportedOperationException("Cannot set value on JTestMapEntry")
  }
}

class JTestMap(vs: (Int, String)*) extends AbstractMap[Int, String] {
  import JTestMap._
  override def entrySet(): JSet[JMap.Entry[Int, String]] = {
    val entrySet = new JHashSet[JMap.Entry[Int, String]](vs.size);
    vs.foreach { case (k, v) => entrySet.add(JTestMapEntry(k, v)) }
    entrySet
  }
}

/** Test that collection wrappers forward equals and hashCode where appropriate. */
class EqualsTest {

  def jListOf(vs: Int*): JList[Int] = new JTestList(vs: _*)
  def jSetOf(vs: Int*): JSet[Int] = new JTestSet(vs: _*)
  def jMapOf(vs: (Int, String)*): JMap[Int, String] = new JTestMap(vs: _*)

  // SeqWrapper extending util.AbstractList inherits equals
  @Test def `Seq as JList has equals`: Unit = {
    def seq = Seq(1, 2, 3)
    def jList = new SeqWrapper(seq)
    assertEquals(jList, jList)
    assertEquals(jListOf(1, 2, 3), jList)
    assertEquals(jList, jListOf(1, 2, 3))
    assertTrue(jList == jListOf(1, 2, 3))
    assertEquals(jList.hashCode, jList.hashCode)
  }

  // SetWrapper extending util.AbstractSet inherits equals
  @Test def `Set as JSet has equals`: Unit = {
    def set = Set(1, 2, 3)
    def jSet = new SetWrapper(set)
    assertEquals(jSet, jSet)
    assertEquals(jSetOf(1, 2, 3), jSet)
    assertEquals(jSet, jSetOf(1, 2, 3))
    assertTrue(jSet == jSetOf(1, 2, 3))
    assertEquals(jSet.hashCode, jSet.hashCode)
  }

  // MapWrapper extending util.AbstractMap inherits equals
  @Test def `Map as JMap has equals`: Unit = {
    def map = Map(1 -> "one", 2 -> "two", 3 -> "three")
    def jMap = new MapWrapper(map)
    assertEquals(jMap, jMap)
    assertEquals(jMapOf(1 -> "one", 2 -> "two", 3 -> "three"), jMap)
    assertEquals(jMap, jMapOf(1 -> "one", 2 -> "two", 3 -> "three"))
    assertTrue(jMap == jMapOf(1 -> "one", 2 -> "two", 3 -> "three"))
    assertEquals(jMap.hashCode, jMap.hashCode)
  }

  @Test def `Iterable as JIterable does not compare equal`: Unit = {
    // scala iterable without element equality defined
    def iterable: Iterable[Int] = new AbstractIterable[Int] {
      override def iterator: Iterator[Int] = Iterator(1, 2, 3)
    }
    def jIterable = new IterableWrapper(iterable)
    assertNotEquals(jIterable, jIterable)
    assertNotEquals(jIterable.hashCode, jIterable.hashCode)
  }

  @Test def `Iterator as JIterator does not compare equal`: Unit = {
    def iterator = Iterator(1, 2, 3)
    def jIterator = new IteratorWrapper(iterator)
    assertNotEquals(jIterator, jIterator)
    assertNotEquals(jIterator.hashCode, jIterator.hashCode)
  }

  @Test def `All wrapper compare equal if underlying is equal`(): Unit = {
    val jList = Collections.emptyList[String]()
    assertEquals(jList.asScala, jList.asScala)

    val jIterator = jList.iterator()
    assertEquals(jIterator.asScala, jIterator.asScala)

    val jEnumeration = Collections.emptyEnumeration[String]()
    assertEquals(jEnumeration.asScala, jEnumeration.asScala)

    val jIterable = jList.asInstanceOf[JIterable[String]]
    assertEquals(jIterable.asScala, jIterable.asScala)

    val jCollection = jList.asInstanceOf[JCollection[String]]
    assertEquals(jCollection.asScala, jCollection.asScala)

    val jSet = Collections.emptySet[String]()
    assertEquals(jSet.asScala, jSet.asScala)

    val jMap = Collections.emptyMap[String, String]()
    assertEquals(jMap.asScala, jMap.asScala)

    val jCMap = new JCMap[String, String]()
    assertEquals(jCMap.asScala, jCMap.asScala)

    val iterator = Iterator.empty[String]
    assertEquals(iterator.asJava, iterator.asJava)

    val iterable = Iterable.empty[String]
    assertEquals(iterable.asJava, iterable.asJava)

    val buffer = mutable.Buffer.empty[String]
    assertEquals(buffer.asJava, buffer.asJava)

    val seq = mutable.Seq.empty[String]
    assertEquals(seq.asJava, seq.asJava)

    val mutableSet = mutable.Set.empty[String]
    assertEquals(mutableSet.asJava, mutableSet.asJava)

    val set = Set.empty[String]
    assertEquals(set.asJava, set.asJava)

    val mutableMap = mutable.Map.empty[String, String]
    assertEquals(mutableMap.asJava, mutableMap.asJava)

    val map = Map.empty[String, String]
    assertEquals(map.asJava, map.asJava)

    val concurrentMap = concurrent.TrieMap.empty[String, String]
    assertEquals(concurrentMap.asJava, concurrentMap.asJava)
  }
}
