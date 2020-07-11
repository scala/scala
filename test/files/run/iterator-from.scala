/* This file tests iteratorFrom, keysIteratorFrom, and valueIteratorFrom on various sorted sets and maps
 * filter: inliner warnings
 */

import scala.util.{Random => R}
import scala.collection._

object Test extends App {
  val maxLength = 25
  val maxKey = 50
  val maxValue = 50

  def testSet[A](set: SortedSet[A], list: List[A])(implicit o: Ordering[A]): Unit = {
    val distinctSorted = list.distinct.sorted
    assertEquals("Set size wasn't the same as list size", set.size, distinctSorted.size)

    for(key <- distinctSorted) {
      val clazz = set.getClass
      val iteratorFrom = (set iteratorFrom key).toList
      check(clazz, list, s"set iteratorFrom $key", s"(set from $key).iterator", iteratorFrom, (set rangeFrom key).iterator.toList)
      check(clazz, list, s"set.iteratorFrom $key", s"distinctSorted dropWhile (_ < $key)", iteratorFrom, distinctSorted dropWhile (o.lt(_, key)))
    }
  }

  def testMap[A, B](map: SortedMap[A, B], list: List[(A, B)])(implicit o: Ordering[A]): Unit = {
    val distinctSorted = distinctByKey(list).sortBy(_._1)
    assertEquals("Map size wasn't the same as list size", map.size, distinctSorted.size)

    for(keyValue <- distinctSorted) {
      val key = keyValue._1
      val clazz = map.getClass
      val iteratorFrom = (map iteratorFrom key).toList
      check(clazz, list, s"map iteratorFrom $key", s"(map from $key).iterator", iteratorFrom, (map rangeFrom key).iterator.toList)
      check(clazz, list, s"map iteratorFrom $key", s"distinctSorted dropWhile (_._1 < $key)", iteratorFrom, distinctSorted dropWhile (x => o.lt(x._1, key)))
      check(clazz, list, s"map iteratorFrom $key map (_._1)", s"map keysIteratorFrom $key", iteratorFrom map (_._1), (map keysIteratorFrom key).toList)
      check(clazz, list, s"map iteratorFrom $key map (_._2)", s"map valuesIteratorFrom $key", iteratorFrom map (_._2), (map valuesIteratorFrom key).toList)
    }
  }

  def check[A](clazz: Class[_], list: List[_], m1: String, m2: String, l1: List[A], l2: List[A]): Unit = {
    assertEquals(s"$clazz: `$m1` didn't match `$m2` on list $list", l1, l2)
  }

  def assertEquals[A](msg: String, x: A, y: A): Unit = {
    assert(x == y, s"$msg\n1: $x\n2: $y")
  }

  def distinctByKey[A,B](list: List[(A, B)]) : List[(A,B)] = list.groupBy(_._1).map(_._2.last).toList

  object Weekday extends Enumeration {
    type Weekday = Value
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  0 until maxLength foreach {length =>
    val keyValues = (0 until length map {_ => (R nextInt maxKey, R nextInt maxValue)}).toList
    val keys = keyValues map (_._2)
    testSet(immutable.BitSet(keys:_*), keys)
    testSet(immutable.TreeSet(keys:_*), keys)
    testSet(mutable.TreeSet(keys:_*), keys)
    val days = keys map {n => Weekday(n % Weekday.values.size)}
    testSet(Weekday.ValueSet(days:_*), days)

    val treeMap = immutable.TreeMap(keyValues:_*)
    testMap(treeMap, keyValues)
    testMap(treeMap.view.filterKeys(_ % 2 == 0).to(SortedMap), keyValues  filter (_._1 % 2 == 0))
    testMap(treeMap.view.mapValues(_ + 1).to(SortedMap), keyValues map {case (k,v) => (k, v + 1)})
  }
}
