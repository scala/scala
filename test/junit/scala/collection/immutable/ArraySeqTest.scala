package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.annotation.nowarn
import scala.reflect.ClassTag

@RunWith(classOf[JUnit4])
class ArraySeqTest {
  @Test
  def slice(): Unit = {

    import language.implicitConversions
    implicit def array2ArraySeq[T](array: Array[T]): ArraySeq[T] =
      ArraySeq.unsafeWrapArray(array)

    val booleanArray = Array(true, false, true, false)
    check(booleanArray, Array(true, false), Array(false, true))

    val shortArray = Array(1.toShort, 2.toShort, 3.toShort, 4.toShort)
    check(shortArray, Array(1.toShort, 2.toShort), Array(2.toShort, 3.toShort))

    val intArray = Array(1, 2, 3, 4)
    check(intArray, Array(1, 2), Array(2, 3))

    val longArray = Array(1L, 2L, 3L, 4L)
    check(longArray, Array(1L, 2L), Array(2L, 3L))

    val byteArray = Array(1.toByte, 2.toByte, 3.toByte, 4.toByte)
    check(byteArray, Array(1.toByte, 2.toByte), Array(2.toByte, 3.toByte))

    val charArray = Array('1', '2', '3', '4')
    check(charArray, Array('1', '2'), Array('2', '3'))

    val doubleArray = Array(1.0, 2.0, 3.0, 4.0)
    check(doubleArray, Array(1.0, 2.0), Array(2.0, 3.0))

    val floatArray = Array(1.0f, 2.0f, 3.0f, 4.0f)
    check(floatArray, Array(1.0f, 2.0f), Array(2.0f, 3.0f))

    val refArray = Array("1", "2", "3", "4")
    check[String](refArray, Array("1", "2"), Array("2", "3"))

    def unit1(): Unit = {}
    def unit2(): Unit = {}
    assertEquals(unit1(), unit2())
    // unitArray is actually an instance of Immutable[BoxedUnit], the check to which is actually checked slice
    // implementation of ofRef
    val unitArray: ArraySeq[Unit] = Array(unit1(), unit2(), unit1(), unit2())
    check(unitArray, Array(unit1(), unit1()), Array(unit1(), unit1()))
  }

  @Test
  def t11187(): Unit = {
    assertEquals(ArraySeq(1, 2), ArraySeq(2, 1).sorted)
    assertEquals(ArraySeq(1, 2), ArraySeq(2, 1).sortBy(identity))
    assertEquals(ArraySeq(1, 2), ArraySeq(2, 1).sortWith(_ < _))
  }

  @Test
  def safeToArray(): Unit = {
    val a = ArraySeq(1,2,3)
    a.toArray.update(0, 100)
    assertEquals(a, List(1,2,3))
  }
  @Test
  def copyToArrayReturnsNonNegative(): Unit = {
    val a = ArraySeq(1,2,3)
    assertEquals(a.copyToArray(Array(1,1,2), 0, -1), 0)
  }

  private def check[T : ClassTag](array: ArraySeq[T], expectedSliceResult1: ArraySeq[T], expectedSliceResult2: ArraySeq[T]): Unit = {
    assertEquals(array, array.slice(-1, 4))
    assertEquals(array, array.slice(0, 5))
    assertEquals(array, array.slice(-1, 5))
    assertEquals(expectedSliceResult1, array.slice(0, 2))
    assertEquals(expectedSliceResult2, array.slice(1, 3))
    assertEquals(ArraySeq.empty[T], array.slice(1, 1))
    assertEquals(ArraySeq.empty[T], array.slice(2, 1))
  }

  @Test def checkSearch(): Unit = SeqTests.checkSearch(ArraySeq(0 to 1000: _*), 15,  implicitly[Ordering[Int]])

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

  private def assertArraySeqAndType[A](actual: ArraySeq[A], expect: ArraySeq[A], expectedArrayType: Class[_]): Unit = {
    assertEquals(actual, expect)
    assertEquals(actual.unsafeArray.getClass(), expectedArrayType)
  }

  @Test
  def appendInt(): Unit = {
    assertArraySeqAndType(ArraySeq(1, 3) :+ 7, ArraySeq(1, 3, 7), classOf[Array[Int]])
  }

  @Test @nowarn("cat=lint-infer-any")
  def appendAny(): Unit = assertArraySeqAndType(ArraySeq(1, 3) :+ "x", ArraySeq[Any](1, 3, "x"), classOf[Array[AnyRef]])

  @Test
  def prependInt(): Unit = {
    assertArraySeqAndType(87 +: ArraySeq(1, 3), ArraySeq(87, 1, 3), classOf[Array[Int]])
  }

  @Test @nowarn("cat=lint-infer-any")
  def prependAny(): Unit = assertArraySeqAndType("x" +: ArraySeq(1, 3), ArraySeq[Any]("x", 1, 3), classOf[Array[AnyRef]])

  @Test
  def updatedInt(): Unit = {
    assertArraySeqAndType(ArraySeq(1, 2).updated(0, 3), ArraySeq(3, 2), classOf[Array[Int]])
  }

  @Test @nowarn("cat=lint-infer-any")
  def updatedAny(): Unit = assertArraySeqAndType(ArraySeq(1, 2).updated(0, "x"), ArraySeq[Any]("x", 2), classOf[Array[AnyRef]])

  @Test
  def foldInt(): Unit = {
    val a = ArraySeq(1, 3)
    assertEquals(a.foldLeft(0)(_ + _), 4)
    assertEquals(a.foldRight(List.empty[Int])(_ :: _), List(1, 3))
  }

  @Test
  def foldString(): Unit = {
    val a = ArraySeq("1", "3")
    assertEquals(a.foldLeft("")(_ + _), "13")
    assertEquals(a.foldRight(List.empty[String])(_ :: _), List("1", "3"))
  }

  @Test
  def foldAny(): Unit = {
    val a = ArraySeq[Any](1, "3")
    assertEquals("13", a.foldLeft("")(_ + _))
    assertEquals(List[Any](1, "3"), a.foldRight(List.empty[Any])(_ :: _))
  }

  @Test
  def from(): Unit = {
    val as = ArraySeq("foo", "bar", "baz")
    assertSame(as, ArraySeq.from(as))
    assertSame(as, ArraySeq(as: _*))
  }

  private def assertConcat[A](lhs: ArraySeq[A], rhs: ArraySeq[A], expect: ArraySeq[A]): Array[_] = {
    val appended = lhs ++ rhs
    val prepended = lhs ++: rhs
    assertEquals(appended, expect)
    assertEquals(prepended, expect)
    assertEquals(appended.unsafeArray.getClass, prepended.unsafeArray.getClass)
    appended.unsafeArray
  }

  @Test
  def concatPrimative(): Unit = {
    val a = ArraySeq(1, 3)
    val b = ArraySeq(5, 7)
    val underlyingArray = assertConcat(a, b, ArraySeq(1, 3, 5, 7))
    assertEquals(underlyingArray.getClass, classOf[Array[Int]])
  }

  @Test
  def concatObject(): Unit = {
    val v = Vector(1)
    val a = ArraySeq[Vector[Int]](v)
    val b = ArraySeq[AnyRef]("hi")
    assertConcat(a, a, ArraySeq(v, v))
    assertConcat(a, b, ArraySeq(v, "hi"))
    assertConcat(b, a, ArraySeq("hi", v))
  }

  @Test
  def concatAny(): Unit = {
    val a = ArraySeq(1)
    val b = ArraySeq("5")
    assertConcat(a, b, ArraySeq[Any](1, "5"))
    assertConcat(b, a, ArraySeq[Any]("5", 1))
  }

  @Test
  def concatEmpty(): Unit = {
    val candidates = Seq[ArraySeq[Any]](
      ArraySeq[Int](1),
      ArraySeq[String]("Nimander"),
      ArraySeq[Any]("Aiden", 123),
      ArraySeq.empty[Any],
      ArraySeq.empty[AnyRef],
      ArraySeq.empty[Int],
      ArraySeq.empty[String],
    )
    for {
      a <- candidates
      b <- candidates
    } {
      val expect = a.toList ++ b.toList
      assertEquals(a ++ b, expect)
      assertEquals(a ++: b, expect)
    }
  }
}
