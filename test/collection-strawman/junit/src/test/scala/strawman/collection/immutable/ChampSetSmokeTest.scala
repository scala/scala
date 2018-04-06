package strawman.collection.immutable

import java.{util => ju}

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import strawman.collection.convert.{DecorateAsJava, DecorateAsScala}

object ChampSetSmokeTest {

  private def emptySet: Set[CustomHashInt] =
    Set.empty[CustomHashInt]

  private def setOf(item: CustomHashInt): Set[CustomHashInt] =
    emptySet + item

  private def setOf(item0: CustomHashInt, item1: CustomHashInt): Set[CustomHashInt] =
    emptySet + item0 + item1

  private def setOf(item0: CustomHashInt, item1: CustomHashInt, item2: CustomHashInt): Set[CustomHashInt] =
    emptySet + item0 + item1 + item2

  def mkValue(value: Int) = new CustomHashInt(value, value)

  def mkValue(value: Int, hash: Int) = new CustomHashInt(value, hash)

}

class ChampSetSmokeTest extends DecorateAsJava with DecorateAsScala {

  import ChampSetSmokeTest._

  @Test def testNodeValNode(): Unit = {
    val input = new ju.LinkedHashMap[Integer, Integer]
    input.put(1, 1)
    input.put(2, 33)
    input.put(3, 3)
    input.put(4, 4)
    input.put(5, 4)
    input.put(6, 6)
    input.put(7, 7)
    input.put(8, 7)

    var set: Set[CustomHashInt] = emptySet
    input.forEach((key, value) => set = set + mkValue(key, value))
    input.forEach((key, value) => assertTrue(set.contains(mkValue(key, value))))
  }

  @Test def testValNodeVal(): Unit = {
    val input = new ju.LinkedHashMap[Integer, Integer]
    input.put(1, 1)
    input.put(2, 2)
    input.put(3, 2)
    input.put(4, 4)
    input.put(5, 5)
    input.put(6, 5)
    input.put(7, 7)

    var set: Set[CustomHashInt] = emptySet
    input.forEach((key, value) => set = set + mkValue(key, value))
    input.forEach((key, value) => assertTrue(set.contains(mkValue(key, value))))
  }

  @Test def testIteration(): Unit = {
    val input = new ju.LinkedHashMap[Integer, Integer]
    input.put(1, 1)
    input.put(2, 2)
    input.put(3, 2)
    input.put(4, 4)
    input.put(5, 5)
    input.put(6, 5)
    input.put(7, 7)

    var set: Set[CustomHashInt] = emptySet
    input.forEach((key, value) => set = set + mkValue(key, value))

    set.foreach(item => input.remove(item.value))
    assertTrue(input.isEmpty)
  }

  @Test def IterateWithLastBitsDifferent(): Unit = {
    val hash_n2147483648_obj1 = mkValue(1, -2147483648)
    val hash_p1073741824_obj2 = mkValue(2, 1073741824)

    val todo: ju.Set[CustomHashInt] = new ju.HashSet[CustomHashInt]
    todo.add(hash_n2147483648_obj1)
    todo.add(hash_p1073741824_obj2)

    val xs: Set[CustomHashInt] = setOf(hash_n2147483648_obj1, hash_p1073741824_obj2)
    xs.forall(todo.remove)

    assertEquals(ju.Collections.EMPTY_SET, todo)
  }

  @Test def TwoCollisionsEquals(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2)
    val ys: Set[CustomHashInt] = setOf(hash98304_obj2, hash98304_obj1)
    assertEquals(xs, ys)
  }

  @Test def ThreeCollisionsEquals(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val hash98304_obj3 = mkValue(3, 98304)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2, hash98304_obj3)
    val ys: Set[CustomHashInt] = setOf(hash98304_obj3, hash98304_obj2, hash98304_obj1)
    assertEquals(xs, ys)
  }

  @Test def RemovalFromCollisonNodeEqualsSingelton(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1)
    val ys: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2) - hash98304_obj2
    assertEquals(xs, ys)
  }

  @Test def CollisionIterate(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)

    val todo: ju.Set[CustomHashInt] = new ju.HashSet[CustomHashInt]
    todo.add(hash98304_obj1)
    todo.add(hash98304_obj2)

    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2)
    xs.forall(todo.remove)

    assertEquals(ju.Collections.EMPTY_SET, todo)
  }

  @Test def CollisionWithMergeInlineAbove1(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val hash268435456_obj3 = mkValue(3, 268435456)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2, hash268435456_obj3) - hash268435456_obj3
    val ys: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2)
    assertEquals(xs, ys)
  }

  @Test def CollisionWithMergeInlineAbove1_2(): Unit = {
    val hash8_obj1 = mkValue(1, 8)
    val hash8_obj2 = mkValue(2, 8)
    val hash268435456_obj3 = mkValue(3, 268435456)
    val xs: Set[CustomHashInt] = setOf(hash8_obj1, hash8_obj2, hash268435456_obj3) - hash268435456_obj3
    val ys: Set[CustomHashInt] = setOf(hash8_obj1, hash8_obj2)
    assertEquals(xs, ys)
  }

  @Test def CollisionWithMergeInlineAbove2(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val hash268435456_obj3 = mkValue(3, 268435456)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash268435456_obj3, hash98304_obj2) - hash268435456_obj3
    val ys: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2)
    assertEquals(xs, ys)
  }

  @Test def CollisionWithMergeInlineAbove2_2(): Unit = {
    val hash8_obj1 = mkValue(1, 8)
    val hash8_obj2 = mkValue(2, 8)
    val hash268435456_obj3 = mkValue(3, 268435456)
    val xs: Set[CustomHashInt] = setOf(hash8_obj1, hash268435456_obj3, hash8_obj2) - hash268435456_obj3
    val ys: Set[CustomHashInt] = setOf(hash8_obj1, hash8_obj2)
    assertEquals(xs, ys)
  }

  @Test def CollisionWithMergeInlineAbove1RemoveOneCollisonNode(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val hash268435456_obj3 = mkValue(3, 268435456)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2, hash268435456_obj3) - hash98304_obj2
    val ys: Set[CustomHashInt] = setOf(hash98304_obj1, hash268435456_obj3)
    assertEquals(xs, ys)
  }

  @Test def CollisionWithMergeInlineAbove2RemoveOneCollisonNode(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val hash268435456_obj3 = mkValue(3, 268435456)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash268435456_obj3, hash98304_obj2) - hash98304_obj2
    val ys: Set[CustomHashInt] = setOf(hash98304_obj1, hash268435456_obj3)
    assertEquals(xs, ys)
  }

  @Test def CollisionWithMergeInlineBelow1(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val hash8_obj3 = mkValue(3, 8)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2, hash8_obj3) - hash8_obj3
    val ys: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2)
    assertEquals(xs, ys)
  }

  @Test def CollisionWithMergeInlineBelow2(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val hash8_obj3 = mkValue(3, 8)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash8_obj3, hash98304_obj2) - hash8_obj3
    val ys: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2)
    assertEquals(xs, ys)
  }

  @Test def CollisionWithMergeInlineBelowRemoveOneCollisonNode1(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val hash8_obj3 = mkValue(3, 8)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash98304_obj2, hash8_obj3) - hash98304_obj2
    val ys: Set[CustomHashInt] = setOf(hash98304_obj1, hash8_obj3)
    assertEquals(xs, ys)
  }

  @Test def CollisionWithMergeInlineBelowRemoveOneCollisonNode2(): Unit = {
    val hash98304_obj1 = mkValue(1, 98304)
    val hash98304_obj2 = mkValue(2, 98304)
    val hash8_obj3 = mkValue(3, 8)
    val xs: Set[CustomHashInt] = setOf(hash98304_obj1, hash8_obj3, hash98304_obj2) - hash98304_obj2
    val ys: Set[CustomHashInt] = setOf(hash98304_obj1, hash8_obj3)
    assertEquals(xs, ys)
  }
}