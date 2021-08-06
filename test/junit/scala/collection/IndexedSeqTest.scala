package scala.collection

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

/**
  * base class for testing common methods on a various implementations
  *
  * @tparam T the collection type
  * @tparam E the element type
  */
abstract class IndexedTest[T, E] {

  protected def size = 10

  /**
    * create a new instance of the test data with known values
    */
  protected def underTest(size: Int): T

  /**
    * returns the value of the data that is expected to be present int the original data at index {{{index}}}
    * This is conceptually the same as the normal apply operation but unavailable due to the base types of T not supporting apply
    *
    * @param index the index to use for the returned value
    * @return the value at the specified index
    */
  protected def expectedValueAtIndex(index: Int): E
  /**
    * check some simple indexed access
    */
  @Test def checkIndexedAccess(): Unit = {
    val test = underTest(size)
    for (i <- 0 until size) {
      assertEquals(expectedValueAtIndex(i), get(test, i), s" at index $i")
    }
  }

  /** check that lengthCompare compares values correctly */
  @Test def checkLengthCompare(): Unit = {
    val test = underTest(size)
    assert(lengthCompare(test, Int.MinValue) > 0)
  }

  /**
    * check simple equality of the initial data.
    * More a test of the infra that we use in this est than a full test of equality
    */
  @Test def checkEquals(): Unit = {
    val test1 = underTest(size)
    val test2 = underTest(size)
    doAssertEquals("", test1, test2)
    assertNotSame(test1, test2)
    expectSameContent("basic equality", false, test1, test2, 0, size)
  }

  protected def expectSameContent(txt: String, canBeSame:Boolean, orig: T, test: T, offset: Int, len: Int): Unit = {
    val txtAndState = s"$txt canBeSame $canBeSame isMutableContent $isMutableContent isTakeAllSame $isTakeAllSame offset $offset len $len length(test) ${length(test)}"
    val isValidSame = canBeSame && !isMutableContent && offset == 0 && len == size
    if (isValidSame && isTakeAllSame)
      assertSame(orig, test, txtAndState)
    else
      assertNotSame(orig, test, txtAndState)
    assertSame(len, length(test), txtAndState)
    for (i <- 0 until len) {
      assertEquals(expectedValueAtIndex(i + offset), get(test, i), s" $txtAndState $i $offset $len")
    }
  }

  /**
    * check the operation of {{{take}}} when the parameter is less than the size of the test data
    */
  @Test def checkTakeNormal(): Unit = {
    val orig = underTest(size)
    for (len <- 0 until size) {
      val taken = take(orig, len)
      expectSameContent(s" len $len", true, orig, taken, 0, len)
    }
  }

  /**
    * check the operation of {{{slice}}} within the bounds of the source
    */
  @Test def checkSliceNormal(): Unit = {
    val orig = underTest(size)
    for (
      from <- 0 until size;
      to <- from until size) {

      val sliced = slice(orig, from, to)
      expectSameContent(s"from $from, to $to", true, orig, sliced, from, to - from)
    }
  }

  /**
    * check the operation of {{{take}}} works for size of 0
    * There is a special case that for some implementations empty will be a singleton
    */
  @Test def checkTakeEmpty(): Unit = {
    val orig = underTest(size)
    val empty1 = take(orig, 0)
    val empty2 = take(orig, 0)
    assertEquals(0, length(empty1))
    if (isEmptyConstant) assertSame(empty1, empty2)
  }

  /**
    * check the operation of {{{slice}}} works for size of 0
    * There is a special case that for some implementations empty will be a singleton
    */
  @Test def checkSliceEmpty(): Unit = {
    val orig = underTest(size)
    for (start <- 0 until size) {
      val empty1 = slice(orig, start, start)
      val empty2 = slice(orig, start, start)
      assertEquals(0, length(empty1), s"start $start")
      if (isEmptyConstant) assertSame(empty1, empty2, s"start $start")
    }
  }

  /**
    * check the operation of {{{take}}} works for the entire content
    * There is a special case that for some immutable implementations they can share the result
    */
  @Test def checkTakeAll(): Unit = {
    val orig = underTest(size)
    val all = take(orig, size)
    assertEquals(size, length(all))
    expectSameContent("take all", true, orig, all, 0, size)
    if (isMutableContent)
      assertNotSame(orig, all)
    else if (isTakeAllSame)
      assertSame(orig, all)
  }

  /**
    * check the operation of {{{slice}}} works for the entire content
    * There is a special case that for some immutable implementations they can share the result
    */
  @Test def checkSliceAll(): Unit = {
    val orig = underTest(size)
    val all = slice(orig, 0, size)
    assertEquals(size, length(all))
    expectSameContent("", true, orig, all, 0, size)
    if (isMutableContent)
      assertNotSame(orig, all)
    else if (isTakeAllSame)
      assertSame(orig, all)
  }

  /**
    * check that take operates appropriately for negative values
    * take and slice should be lenient and silently ignore any data outside valid ranges
    */
  @Test def checkTakeNeg(): Unit = {
    val orig = underTest(size)
    val e = take(orig, 0)
    for (len <- List(-1, -10, -99, Int.MinValue)) {
      val empty = take(orig, len)
      assertEquals(0, length(empty), s"len $len")
      if (isEmptyConstant) assertSame(e, empty, s"len $len")
    }
  }

  /**
    * check that take operates appropriately for lengths that exceed the input size
    * take and slice should be lenient and silently ignore any data outside valid ranges
    */
  @Test def checkTakeTooBig(): Unit = {
    val orig = underTest(size)
    val e = take(orig, 0)
    assertNotNull(e)
    for (len <- List(size + 1, size + 10, Int.MaxValue)) {
      val all = take(orig, len)
      assertEquals(size, length(all), s"len $len")
      expectSameContent("", true, orig, all, 0, size)
    }
  }

  /**
    * check that slice operates appropriately for negative start point
    * take and slice should be lenient and silently ignore any data outside valid ranges
    */
  @Test def checkSliceFromNeg(): Unit = {
    val orig = underTest(size)
    for (
      from <- List(-1, -10, -99, Int.MinValue);
      to <- List(-1, 0, 1, 5)) {
      val start = slice(orig, from, to)
      expectSameContent(s"from $from, to $to", true, orig, start, 0, Math.max(0, to))
    }
  }

  /**
    * check that slice operates appropriately for out of range end values
    * take and slice should be lenient and silently ignore any data outside valid ranges
    */
  @Test def checkSliceToTooBig(): Unit = {
    val orig = underTest(size)
    for (
      from <- List(-1, -10, -99, Int.MinValue, 0, 1, 5);
      to <- List(size + 1, size + 10, Int.MaxValue)) {
      val start = slice(orig, from, to)
      val realStart = Math.max(0, from)
      val realLen = size - realStart
      expectSameContent(s"from $from, to $to", true, orig, start, realStart, realLen)
    }
  }

  /**
    * check that slice operates appropriately for negative values start and ends too large
    * take and slice should be lenient and silently ignore any data outside valid ranges
    */
  @Test def checkSliceFromNegAndToTooBig(): Unit = {
    val orig = underTest(size)
    for (
      from <- List(-1, -10, -99, Int.MinValue);
      to <- List(size + 1, size + 10, Int.MaxValue)) {
      val all = slice(orig, from, to)
      expectSameContent(s"from $from, to $to", true, orig, all, 0, size)
    }
  }

  protected def intercept[EX <: Exception : Manifest](fn: => Any): Unit = {
    try {
      val res = fn
      fail(s"expected exception was not thrown: $res")
    } catch {
      case failed: AssertionError => throw failed
      case e: Exception if manifest[EX].runtimeClass.isAssignableFrom(e.getClass) =>
    }
  }

  //accessors
  //the length of underTest
  def length(underTest: T): Int

  def lengthCompare(underTest: T, len: Int): Int

  //the value at index i of underTest
  def get(underTest: T, i: Int): E

  def slice(underTest: T, from: Int, to: Int): T

  def take(underTest: T, size: Int): T

  //behaviour
  /** is an empty value the same JVM instance */
  def isEmptyConstant: Boolean

  /** is a take / slice that results in all the data returned return this
    * This is only relevant if !isMutableContent
    */
  def isTakeAllSame: Boolean

  /** is the content of the collection mutable.
    * If mutable there is not data sharing allowed by take/slice, if immutable then data sharing is possible
    * and tested based on isTakeAllSame
    */
  def isMutableContent: Boolean

  //helpers
  //delegate equals check for support arrays
  def doAssertEquals(txt: String, expected: T, actual: T): Unit

}
package IndexedTestImpl {
  import java.lang.reflect.{Array => jlArray}
  import java.lang.{Boolean => jlBoolean}
  import java.lang.{Byte => jlByte}
  import java.lang.{Short => jlShort}
  import java.lang.{Integer => jlInt}
  import java.lang.{Long => jlLong}
  import java.lang.{Float => jlFloat}
  import java.lang.{Double => jlDouble}
  import java.lang.{Character => jlChar}

  import scala.collection.immutable.{StringOps, WrappedString}
  import scala.collection.mutable
  import scala.runtime.BoxedUnit
  trait DataProvider[E] {
    protected def expectedValueAtIndex(index: Int): E = {
      val someNumber = index + jlInt.bitCount(index)
      toType(someNumber)
    }

    protected def toType(n: Int): E
  }
  trait StringTestData extends DataProvider [String] {
    def toType(n: Int) = n.toString
  }
  trait ByteTestData extends DataProvider [Byte] {
    def toType(n: Int) = n.toByte
  }
  trait ShortTestData extends DataProvider [Short] {
    def toType(n: Int) = n.toShort
  }
  trait IntTestData extends DataProvider [Int] {
    def toType(n: Int) = n
  }
  trait LongTestData extends DataProvider [Long] {
    def toType(n: Int) = n
  }
  trait FloatTestData extends DataProvider [Float] {
    def toType(n: Int) = n.toFloat
  }
  trait DoubleTestData extends DataProvider [Double] {
    def toType(n: Int) = n
  }
  trait CharTestData extends DataProvider [Char] {
    def toType(n: Int)= (n+64).toChar
  }
  trait BooleanTestData extends DataProvider [Boolean] {
    def toType(n: Int)= (n & 0) == 0
  }
  trait UnitTestData extends DataProvider [BoxedUnit] {
    def toType(n: Int)= if ((n & 0) == 0) null else BoxedUnit.UNIT
  }

  abstract class ArrayTest[E] (
                               //the object or primitive type of the array
                               val TYPE: Class[_]) extends IndexedTest[Array[E], E]{
    override final def length(underTest: Array[E]) = underTest.length

    override final def lengthCompare(underTest: Array[E], len: Int): Int = underTest.lengthCompare(len)

    override def get(underTest: Array[E], i: Int) = underTest(i)

    override def slice(underTest: Array[E], from: Int, to: Int) = underTest.slice(from, to)

    override def take(underTest: Array[E], size: Int) = underTest.take(size)

    override def isEmptyConstant = false

    override def isMutableContent = true

    override def isTakeAllSame = false

    override def doAssertEquals(txt: String, expected: Array[E], actual: Array[E]): Unit = {
      assertEquals(txt, expected.mkString("'"), actual.mkString("'"))
    }

    override def underTest(size: Int): Array[E] = {
      val res = jlArray.newInstance(TYPE, size)
      for (i <- 0 until size) {
        jlArray.set(res, i, expectedValueAtIndex(i))
      }
      res.asInstanceOf[Array[E]]
    }
  }

  abstract class ArraySeqTest[E](
                                      //the object or primitive type of the array
                                      val TYPE: Class[_]) extends IndexedTest[mutable.ArraySeq[E], E]  with DataProvider[E]{
    import mutable.ArraySeq
    override final def length(underTest: ArraySeq[E]) = underTest.length

    override final def lengthCompare(underTest: ArraySeq[E], len: Int): Int = underTest.lengthCompare(len)

    override def get(underTest: ArraySeq[E], i: Int) = underTest(i)

    override def slice(underTest: ArraySeq[E], from: Int, to: Int) = underTest.slice(from, to)

    override def take(underTest: ArraySeq[E], size: Int) = underTest.take(size)

    override def isEmptyConstant = false

    override def isMutableContent = true

    override def isTakeAllSame = false

    override def doAssertEquals(txt: String, expected: ArraySeq[E], actual: ArraySeq[E]): Unit = {
      assertEquals(txt, expected.mkString("'"), actual.mkString("'"))
    }

    override def underTest(size: Int): ArraySeq[E] = {
      val res = jlArray.newInstance(TYPE, size)
      for (i <- 0 until size) {
        jlArray.set(res, i, expectedValueAtIndex(i))
      }
      ArraySeq.make(res.asInstanceOf[Array[E]])
    }
  }

  //construct the data using java as much as possible to avoid invalidating the test

  abstract class MutableIndexedSeqTest[T <: mutable.Seq[E], E] extends IndexedTest[T, E]   with DataProvider[E]{
    override final def length(underTest: T) = underTest.length

    override final def lengthCompare(underTest: T, len: Int): Int = underTest.lengthCompare(len)

    override def get(underTest: T, i: Int) = underTest(i)

    override def slice(underTest: T, from: Int, to: Int) = underTest.slice(from, to).asInstanceOf[T]

    override def take(underTest: T, size: Int) = underTest.take(size).asInstanceOf[T]

    override def isEmptyConstant = false

    override def isMutableContent = true

    override def isTakeAllSame = true

    override def doAssertEquals(txt: String, expected: T, actual: T): Unit = {
      assertEquals(expected, actual, txt)
    }

    def createEmpty(size: Int) : T

    override protected def underTest(size: Int): T = {
      val res:T  = createEmpty(size)
      for (i <- 0 until size)
        res(i) = expectedValueAtIndex(i)
      res
    }

  }

  abstract class ImmutableIndexedSeqTest[T <: SeqOps[E, Seq, T], E] extends IndexedTest[T, E]   with DataProvider[E] {
    override final def length(underTest: T) = underTest.length

    override final def lengthCompare(underTest: T, len: Int): Int = underTest.lengthCompare(len)

    override def get(underTest: T, i: Int) = underTest(i)

    override def slice(underTest: T, from: Int, to: Int) = underTest.slice(from, to)

    override def take(underTest: T, size: Int) = underTest.take(size)

    override def isEmptyConstant = false

    override def isMutableContent = false

    override def isTakeAllSame = true

    override def doAssertEquals(txt: String, expected: T, actual: T): Unit = {
      assertEquals(expected, actual, txt)
    }

  }

  abstract class StringOpsBaseTest extends IndexedTest[StringOps, Char] with DataProvider[Char]  {
    override final def length(underTest: StringOps) = underTest.size

    override final def lengthCompare(underTest: StringOps, len: Int): Int = underTest.lengthCompare(len)

    override def get(underTest: StringOps, i: Int) = underTest(i)

    override def slice(underTest: StringOps, from: Int, to: Int) = underTest.slice(from, to)

    override def take(underTest: StringOps, size: Int) = underTest.take(size)

    override def isEmptyConstant = false

    override def isMutableContent = false

    override def isTakeAllSame = false

    override def doAssertEquals(txt: String, expected: StringOps, actual: StringOps): Unit = {
      assertEquals(expected, actual, txt)
    }

  }

  class BooleanArrayTest extends ArrayTest[Boolean](jlBoolean.TYPE) with BooleanTestData
  class ByteArrayTest extends ArrayTest[Byte](jlByte.TYPE) with ByteTestData
  class ShortArrayTest extends ArrayTest[Short](jlShort.TYPE) with ShortTestData
  class IntArrayTest extends ArrayTest[Int](jlInt.TYPE) with IntTestData
  class LongArrayTest extends ArrayTest[Long](jlLong.TYPE) with LongTestData
  class DoubleArrayTest extends ArrayTest[Double](jlDouble.TYPE) with DoubleTestData
  class FloatArrayTest extends ArrayTest[Float](jlFloat.TYPE) with FloatTestData
  class CharArrayTest extends ArrayTest[Char](jlChar.TYPE) with CharTestData
  class UnitArrayTest extends ArrayTest[BoxedUnit](null) with UnitTestData {
    override def underTest(size: Int): Array[BoxedUnit] = {
      val res = new Array[Unit](size)
      for (i <- 0 until size) {
        jlArray.set(res, i, expectedValueAtIndex(i))
      }
      res.asInstanceOf[Array[BoxedUnit]]
    }
  }
  class RefArrayTest extends ArrayTest[String](classOf[String]) with StringTestData

  class BooleanArraySeqTest extends ArraySeqTest[Boolean](jlBoolean.TYPE) with BooleanTestData
  class ByteArraySeqTest extends ArraySeqTest[Byte](jlByte.TYPE) with ByteTestData
  class ShortArraySeqTest extends ArraySeqTest[Short](jlShort.TYPE) with ShortTestData
  class IntArraySeqTest extends ArraySeqTest[Int](jlInt.TYPE) with IntTestData
  class LongArraySeqTest extends ArraySeqTest[Long](jlLong.TYPE) with LongTestData
  class DoubleArraySeqTest extends ArraySeqTest[Double](jlDouble.TYPE) with DoubleTestData
  class FloatArraySeqTest extends ArraySeqTest[Float](jlFloat.TYPE) with FloatTestData
  class CharArraySeqTest extends ArraySeqTest[Char](jlChar.TYPE) with CharTestData
  class UnitArraySeqTest extends ArraySeqTest[BoxedUnit](null) with UnitTestData {
    import mutable.ArraySeq
    override def underTest(size: Int): ArraySeq[BoxedUnit] = {
      val res = new Array[Unit](size)
      for (i <- 0 until size) {
        jlArray.set(res, i, expectedValueAtIndex(i))
      }
      ArraySeq.make(res).asInstanceOf[ArraySeq[BoxedUnit]]
    }
  }
  class RefArraySeqTest extends ArraySeqTest[String](classOf[String]) with StringTestData

  class ListBufferTest extends MutableIndexedSeqTest[mutable.ListBuffer[String], String]  with StringTestData {
    import mutable.ListBuffer
    override def createEmpty(size: Int): ListBuffer[String] = {
      val res = new ListBuffer[String]
      for (i <- 0 until size)
        res += expectedValueAtIndex(i)
      res
    }
  }
  /*class ArraySeqTest extends MutableIndexedSeqTest[mutable.ArraySeq[String], String]  with StringTestData {
    import mutable.ArraySeq
    override def createEmpty(size: Int): ArraySeq[String] = {
      val res = new ArraySeq[String](size)
      for (i <- 0 until size)
        res (i) = expectedValueAtIndex(i)
      res
    }
  }*/
  class ArrayBufferTest extends MutableIndexedSeqTest[mutable.ArrayBuffer[String], String]  with StringTestData {
    import mutable.ArrayBuffer
    override def createEmpty(size: Int): ArrayBuffer[String] = {
      val res = new ArrayBuffer[String](size)
      for (i <- 0 until size)
        res += expectedValueAtIndex(i)
      res
    }
  }
  class ListTest extends ImmutableIndexedSeqTest[List[String], String]  with StringTestData {

    override protected def underTest(size: Int): List[String] = {
      var res:List[String] = Nil
      var index = size-1
      while (index >= 0) {
        res = expectedValueAtIndex(index) :: res
        index -= 1
      }
      res
    }
  }
  class StringBuilderTest extends MutableIndexedSeqTest[StringBuilder, Char]  with CharTestData {

    override def createEmpty(size: Int): StringBuilder = new StringBuilder(size)

    override protected def underTest(size: Int): StringBuilder = {
      val res = createEmpty(size)
      for (i <- 0 until size)
        res += expectedValueAtIndex(i)
      res
    }
  }
  class StringOpsTest extends StringOpsBaseTest with CharTestData {

    override protected def underTest(size: Int): StringOps = {
      val res = new StringBuilder(size)
      for (i <- 0 until size)
        res += expectedValueAtIndex(i)
      res.toString
    }
  }
  class WrappedStringTest extends ImmutableIndexedSeqTest[WrappedString, Char]  with CharTestData {

    override def isTakeAllSame: Boolean = false

    override protected def underTest(size: Int):  WrappedString = {
      val res = new StringBuilder(size)
      for (i <- 0 until size)
        res += expectedValueAtIndex(i)
      new WrappedString(res.toString)
    }
  }
  class VectorTest extends ImmutableIndexedSeqTest[Vector[String], String]  with StringTestData {

    override protected def underTest(size: Int): Vector[String] = {
      val res = Vector.newBuilder[String]
      for (i <- 0 until size)
        res += expectedValueAtIndex(i)
      res.result()
    }
  }

}
