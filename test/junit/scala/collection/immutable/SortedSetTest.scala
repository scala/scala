package scala.collection.immutable

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.testkit.AllocationTest
import scala.tools.testkit.AssertUtil.assertThrows


class SortedSetTest extends AllocationTest{

  @Test def empty(): Unit ={
    val ord = Ordering[String]
    exactAllocates(24)(SortedSet.empty(ord))
  }
  @Test def apply0(): Unit ={
    val ord = Ordering[String]
    exactAllocates(24)(SortedSet()(ord))
  }
  @Test def apply1(): Unit ={
    val ord = Ordering[String]
    exactAllocates(152)(SortedSet("a")(ord))
  }
  @Test def apply2(): Unit ={
    val ord = Ordering[String]
    exactAllocates(216)(SortedSet("a", "b")(ord))
  }

  @Test def redBlackValidate(): Unit = {
    import RedBlackTree._
    def redLeaf(x: Int) = RedTree(x, null, null, null)
    def blackLeaf(x: Int) = BlackTree(x, null, null, null)

    validate(redLeaf(1))
    validate(blackLeaf(1))
    assertThrows[AssertionError](validate(RedTree(2, null, redLeaf(1), null)), _.contains("red-red"))
    assertThrows[AssertionError](validate(RedTree(2, null, blackLeaf(1), null)), _.contains("not balanced"))
    validate(RedTree(2, null, blackLeaf(1), blackLeaf(3)))
    validate(BlackTree(2, null, blackLeaf(1), blackLeaf(3)))
    assertThrows[AssertionError](validate(RedTree(4, null, blackLeaf(1), blackLeaf(3))), _.contains("key check"))
  }

  @Test def t12921(): Unit = {
    val s1 = TreeSet(6, 1, 11, 9, 10, 8)
    RedBlackTree.validate(s1.tree)

    val s2 = s1.rangeFrom(2)
    RedBlackTree.validate(s2.tree)
    assertEquals(Set(6, 8, 9, 10, 11), s2)

    val s3 = s2 ++ Seq(7,3,5)
    RedBlackTree.validate(s3.tree)
    assertEquals(Set(3, 5, 6, 7, 8, 9, 10, 11), s3)

    val s4 = s3.rangeFrom(4)
    RedBlackTree.validate(s4.tree)
    assertEquals(Set(5, 6, 7, 8, 9, 10, 11), s4)
  }

  @Test def t12921b(): Unit = {
    import RedBlackTree._
    val t = BlackTree(
      5,
      null,
      BlackTree(
        3,
        null,
        RedTree(1, null, null, null),
        RedTree(4, null, null, null)
      ),
      BlackTree(7, null, RedTree(6, null, null, null), null)
    )
    validate(t)
    validate(from(t, 2))
  }

  @Test def t12921c(): Unit = {
    import RedBlackTree._
    val t = BlackTree(
      8,
      null,
      BlackTree(4, null, null, RedTree(6, null, null, null)),
      BlackTree(
        12,
        null,
        RedTree(10, null, null, null),
        RedTree(14, null, null, null)
      )
    )
    validate(t)
    validate(to(t, 13))
  }

  @Test def t12921d(): Unit = {
    import RedBlackTree._
    val t = BlackTree(
      8,
      null,
      BlackTree(4, null, null, RedTree(6, null, null, null)),
      BlackTree(
        12,
        null,
        RedTree(10, null, null, null),
        RedTree(14, null, null, null)
      )
    )
    validate(t)
    validate(until(t, 13))
  }
}
