package scala.collection.immutable

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.testkit.AllocationTest
import scala.util.hashing.MurmurHash3


class MapHashcodeTest extends AllocationTest {

  @Test def nonAllocatingMapN(): Unit = {
    val t0 = Map.empty
    val t1 = new Map.Map1(1, 1)
    val t2 = new Map.Map2(1, 1, 2, 2)
    val t3 = new Map.Map3(1, 1, 2, 2, 3, 3)
    val t4 = new Map.Map4(1, 1, 2, 2, 3, 3, 4, 4)

    nonAllocating {
      t0.hashCode()
    }
    nonAllocating {
      t0.##
    }

    nonAllocating {
      t1.hashCode()
    }
    nonAllocating {
      t1.##
    }

    nonAllocating {
      t2.hashCode()
    }
    nonAllocating {
      t2.##
    }

    nonAllocating {
      t3.hashCode()
    }
    nonAllocating {
      t3.##
    }

    nonAllocating {
      t4.hashCode()
    }
    nonAllocating {
      t4.##
    }
  }

  @Test def nonAllocatingListMap(): Unit = {
    val t0 = ListMap.empty[String, String]
    val t1 = t0.updated("1", "1")
    val t2 = t1.updated("2", "2")
    val t3 = t2.updated("3", "3")
    val t4 = t3.updated("4", "4")

    val tlarge = (1 to 10000).foldLeft(t0) {
      (a, b) => a.updated(b.toString, b.toString)
    }

    nonAllocating {
      t0.hashCode()
    }
    nonAllocating {
      t0.##
    }

    onlyAllocates(64) {
      t1.hashCode()
    }
    onlyAllocates(64) {
      t1.##
    }

    onlyAllocates(64) {
      t2.hashCode()
    }
    onlyAllocates(64) {
      t2.##
    }

    onlyAllocates(64) {
      t3.hashCode()
    }
    onlyAllocates(64) {
      t3.##
    }

    onlyAllocates(64) {
      t4.hashCode()
    }
    onlyAllocates(64) {
      t4.##
    }

    onlyAllocates(64) {
      tlarge.hashCode()
    }
    onlyAllocates(64) {
      tlarge.##
    }
  }

  @Test def nonAllocatingSortedMap(): Unit = {
    val t0 = SortedMap.empty[String, String]
    val t1 = t0.updated("1", "1")
    val t2 = t1.updated("2", "2")
    val t3 = t2.updated("3", "3")
    val t4 = t3.updated("4", "4")

    val tlarge = (1 to 10000).foldLeft(t0) {
      (a, b) => a.updated(b.toString, b.toString)
    }

    onlyAllocates(32) {
      t0.hashCode()
    }
    onlyAllocates(32) {
      t0.##
    }

    onlyAllocates(32) {
      t1.hashCode()
    }
    onlyAllocates(32) {
      t1.##
    }

    onlyAllocates(32) {
      t2.hashCode()
    }
    onlyAllocates(32) {
      t2.##
    }

    onlyAllocates(32) {
      t3.hashCode()
    }
    onlyAllocates(32) {
      t3.##
    }

    onlyAllocates(32) {
      t4.hashCode()
    }
    onlyAllocates(32) {
      t4.##
    }

    onlyAllocates(32) {
      tlarge.hashCode()
    }
    onlyAllocates(32) {
      tlarge.##
    }
  }

  @Test def nonAllocatingHashMap(): Unit = {
    val t0 = HashMap.empty[String, String]
    val t1 = t0.updated("1", "1")
    val t2 = t1.updated("2", "2")
    val t3 = t2.updated("3", "3")
    val t4 = t3.updated("4", "4")

    val tlarge = (1 to 10000).foldLeft(t0) {
      (a, b) => a.updated(b.toString, b.toString)
    }

    nonAllocating {
      t0.hashCode()
    }
    nonAllocating {
      t0.##
    }

    onlyAllocates(256) {
      t1.hashCode()
    }
    onlyAllocates(256) {
      t1.##
    }

    onlyAllocates(256) {
      t2.hashCode()
    }
    onlyAllocates(256) {
      t2.##
    }

    onlyAllocates(256) {
      t3.hashCode()
    }
    onlyAllocates(256) {
      t3.##
    }

    onlyAllocates(256) {
      t4.hashCode()
    }
    onlyAllocates(256) {
      t4.##
    }

    onlyAllocates(256) {
      tlarge.hashCode()
    }
    onlyAllocates(256) {
      tlarge.##
    }
  }

  object MyEmptyMap extends AbstractMap[String, String] {
    override def get(key: String): Option[String] = None
    override def iterator: Iterator[(String, String)] = Iterator.empty
    override def removed(key: String): Map[String, String] = ???
    override def updated[V1 >: String](key: String, value: V1): Map[String, V1] = ???
  }

  @Test def emptyHashCodes(): Unit = {
    val expected = MyEmptyMap.hashCode()
    assertEquals(expected, Map.empty.hashCode())
    assertEquals(expected, ListMap.empty.hashCode())
    assertEquals(expected, SortedMap.empty[String, Object].hashCode())
    assertEquals(expected, HashMap.empty.hashCode())
  }

  private def assertConsistent(m: Map[_, _]): Unit = {
    assertEquals(m.getClass.toString, MurmurHash3.mapHash(m), m.hashCode())
    assertEquals(m.getClass.toString, MurmurHash3.unorderedHash(m, MurmurHash3.mapSeed), m.hashCode())
  }

  @Test
  def mapNConsistent(): Unit = {
    val t0 = Map.empty[String, String]
    val t1 = t0.updated("1", "1")
    val t2 = t1.updated("2", "2")
    val t3 = t2.updated("3", "3")
    val t4 = t3.updated("4", "4")
    assertConsistent(t0)
    assertConsistent(t1)
    assertConsistent(t2)
    assertConsistent(t3)
    assertConsistent(t4)
  }

  @Test
  def listMapConsistent(): Unit = {
    val t0 = ListMap.empty[String, String]
    val t1 = t0.updated("1", "1")
    val t2 = t1.updated("2", "2")
    val t3 = t2.updated("3", "3")
    val t4 = t3.updated("4", "4")
    assertConsistent(t0)
    assertConsistent(t1)
    assertConsistent(t2)
    assertConsistent(t3)
    assertConsistent(t4)
  }

  @Test
  def sortedMapConsistent(): Unit = {
    val t0 = SortedMap.empty[String, String]
    val t1 = t0.updated("1", "1")
    val t2 = t1.updated("2", "2")
    val t3 = t2.updated("3", "3")
    val t4 = t3.updated("4", "4")
    assertConsistent(t0)
    assertConsistent(t1)
    assertConsistent(t2)
    assertConsistent(t3)
    assertConsistent(t4)
  }

  @Test
  def hashMapConsistent(): Unit = {
    val t0 = HashMap.empty[String, String]
    val t1 = t0.updated("1", "1")
    val t2 = t1.updated("2", "2")
    val t3 = t2.updated("3", "3")
    val t4 = t3.updated("4", "4")
    assertConsistent(t0)
    assertConsistent(t1)
    assertConsistent(t2)
    assertConsistent(t3)
    assertConsistent(t4)
  }
}
