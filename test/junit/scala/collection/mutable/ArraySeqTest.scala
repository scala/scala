package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.immutable.Seq
import scala.reflect.ClassTag

@RunWith(classOf[JUnit4])
class ArraySeqTest {
  @Test
  def t11187(): Unit = {
    assertEquals(ArraySeq(1, 2), ArraySeq(2, 1).sorted)
  }
  @Test
  def t10851(): Unit = {
    val s1 = ArraySeq.untagged(1,2,3)
    assertTrue(s1.array.getClass == classOf[Array[AnyRef]])
    val s2 = ArraySeq.make(Array(1))
    assertTrue(s2.array.getClass == classOf[Array[Int]])
    val s3 = ArraySeq.make(Array(1): Array[Any]).asInstanceOf[ArraySeq[Int]]
    assertTrue(s3.array.getClass == classOf[Array[AnyRef]])
  }

  @Test
  def safeToArray(): Unit = {
    val a = ArraySeq(1,2,3)
    a.toArray.update(0, 100)
    assertEquals(a, List(1,2,3))
  }

  @Test
  def testSortInPlaceAnyRef: Unit = {
    val arr = ArraySeq[Integer](3, 2, 1)
    arr.sortInPlace()
    assertEquals(ArraySeq[Integer](1, 2, 3), arr)
  }

  @Test
  def testSortInPlaceInt: Unit = {
    val arr = ArraySeq.make(Array[Int](3, 2, 1))
    arr.sortInPlace()
    assertEquals(ArraySeq.make(Array[Int](1, 2, 3)), arr)
  }

  @Test
  def testCooperativeEquality(): Unit = {
    assertEquals(ArraySeq(1, 2, 3), ArraySeq(1L, 2L, 3L))
    assertEquals(ArraySeq(1, 2) :+ 3, ArraySeq(1L, 2L) :+ 3L) // :+ makes it an ArraySeq.ofRef
  }

  @Test
  def t10690(): Unit = {
    val x = Seq[Byte](10)
    val y = Array[Byte](10).toSeq
    assertEquals(x.hashCode(), y.hashCode())
  }

  @Test
  def ofRefEquality(): Unit = {
    def assertOfRef(left: Array[AnyRef], right: Array[AnyRef]): Unit = {
      assert(new ArraySeq.ofRef(left) == new ArraySeq.ofRef(right))
    }
    assertOfRef(Array(Int.box(65)), Array(Double.box(65.0)))
    assertOfRef(Array(Double.box(65.0)), Array(Int.box(65)))
    assertOfRef(Array(Int.box(65)), Array(Char.box('A')))
    assertOfRef(Array(Char.box('A')), Array(Int.box(65)))
  }
}

/*
scala> import scala.collection.mutable.WrappedArray
import scala.collection.mutable.WrappedArray

scala> val a = WrappedArray.make(Array(1))
a: scala.collection.mutable.WrappedArray[Int] = WrappedArray(1)

scala> a.array.getClass
res0: Class[_ <: Array[Int]] = class [I

scala> val a = WrappedArray.make(Array(1): Array[Any]).asInstanceOf[WrappedArray[Int]]
a: scala.collection.mutable.WrappedArray[Int] = WrappedArray(1)

scala> a.array.getClass
res1: Class[_ <: Array[Int]] = class [Ljava.lang.Object;
 */