
package scala.collection.convert

import org.junit.Test
import org.junit.Assert._

import scala.jdk.CollectionConverters._
import JavaCollectionWrappers._

import java.util.{AbstractList, AbstractSet, List => JList, Set => JSet}

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

/** Test that collection wrappers forward equals and hashCode where appropriate. */
class EqualsTest {

  def jlstOf(vs: Int*): JList[Int] = new JTestList(vs: _*)
  def jsetOf(vs: Int*): JSet[Int] = new JTestSet(vs: _*)

  // Seq extending AbstractList inherits equals

  @Test def `List as JList has equals`: Unit = {
    val list = List(1, 2, 3)
    val jlst = new SeqWrapper(list)
    assertEquals(jlstOf(1, 2, 3), jlst)
    assertEquals(jlst, jlstOf(1, 2, 3))
    assertTrue(jlst == jlstOf(1, 2, 3))
    assertEquals(jlst.hashCode, jlst.hashCode)
  }

  @Test def `Set as JSet has equals`: Unit = {
    val set = Set(1, 2, 3)
    val jset = new SetWrapper(set)
    assertEquals(jsetOf(1, 2, 3), jset)
    assertEquals(jset, jsetOf(1, 2, 3))
    assertTrue(jset == jsetOf(1, 2, 3))
    assertEquals(jset.hashCode, jset.hashCode)
  }

  @Test def `Map as JMap has equals`: Unit = {
    val map = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val jmap = new MapWrapper(map)
    assertEquals(jmap, jmap)
  }

  @Test def `Anything as Collection is equal to Anything`: Unit = {
    def set = Set(1, 2, 3)
    def jset = new IterableWrapper(set)
    assertTrue(jset == jset)
    assertEquals(jset, jset)
    assertNotEquals(jset, set)
    assertEquals(jset.hashCode, jset.hashCode)
  }

  @Test def `Iterator wrapper does not compare equal`: Unit = {
    def it = List(1, 2, 3).iterator
    def jit = new IteratorWrapper(it)
    assertNotEquals(jit, jit)
    assertNotEquals(jit.hashCode, jit.hashCode)
  }

  @Test def `Anything.asScala Iterable has case equals`: Unit = {
    def vs = jlstOf(42, 27, 37)
    def it = new JListWrapper(vs)
    assertEquals(it, it)
    assertEquals(it.hashCode, it.hashCode)
  }
}
