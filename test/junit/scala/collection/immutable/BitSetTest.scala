package scala.collection.immutable

import java.util
import java.util.NoSuchElementException

import org.junit.Test
import org.junit.Assert._

class BitSetTest {
  val datasets =
    List(
      List(),
      List(0),
      List(1),
      List(2),
      List(1, 3, 5, 6, 7, 8, 9),
      List(63, 64, 65, 10008),
      List(64, 65, 10008),
      List(65, 10008),
      List(1, 100001),
      List(99),
      List(640)
      )
  @Test def testBuilder(): Unit = {
    for (bulk1 <- List(true, false);
         bulk2 <- List(true, false);
         ds1 <- datasets;
         ds2 <- datasets) {
      val testBuilder1    = BitSet.newBuilder
      val controlBuilder1 = TreeSet.newBuilder[Int]

      if (bulk1) testBuilder1 ++= ds1
      else ds1 foreach testBuilder1.+=
      if (bulk2) testBuilder1 ++= ds2
      else ds2 foreach testBuilder1.+=

      controlBuilder1 ++= ds1
      controlBuilder1 ++= ds2

      assertEquals(s"ds1 $ds1 ds2 $ds2 bulk1 $bulk1 bulk2 $bulk2",
                   controlBuilder1.result.mkString("[", " : ", "]"),
                   testBuilder1.result.mkString("[", " : ", "]"))


    }
  }

  def testIterator(data: List[Int]): Unit = {
    val testBuilder = BitSet.newBuilder
    testBuilder ++= data
    val underTest = testBuilder.result()

    assertEquals(data, underTest.iterator.toList)
  }
  def testIteratorFrom(data: List[Int], from: Int): Unit = {
    val testBuilder = BitSet.newBuilder
    testBuilder ++= data
    val underTest = testBuilder.result()

    assertEquals(s"data $data, from:$from", data.dropWhile(_ < from), underTest.iteratorFrom(from).toList)
  }
  @Test def iteratorNext(): Unit = {
    val testBuilder = BitSet.newBuilder
    testBuilder ++= List(1, 5, 4)
    val underTest = testBuilder.result()

    var it = underTest.iterator
    assertEquals(1, it.next)
    assertEquals(4, it.next)
    assertEquals(5, it.next)
    try {
      it.next()
      fail()
    } catch {
      case _: NoSuchElementException =>
    }

    it = underTest.iterator
    assertTrue(it.hasNext)
    assertTrue(it.hasNext)
    assertEquals(1, it.next)
    assertEquals(4, it.next)
    assertTrue(it.hasNext)
    assertTrue(it.hasNext)
    assertTrue(it.hasNext)
    assertEquals(5, it.next)
    assertFalse(it.hasNext)
    try {
      it.next()
      fail()
    } catch {
      case _: NoSuchElementException =>
    }
  }

  @Test def iterator(): Unit = {
    datasets foreach testIterator
  }

  @Test def iteratorFrom(): Unit = {
    for (from <- -1 to 10;
         ds <- datasets) {
      testIteratorFrom(ds, from)
    }
  }
  @Test def isEmpty(): Unit = {
    val data = new Array[Long](10)
    for (length <- 0 to 10) {
      val bs = BitSet.fromBitMask(data.take(length))
      assertTrue(bs.isEmpty)
      assertEquals(0, bs.size)
      assertTrue(bs.hasDefiniteSize)
    }
    for (length <- 1 to 10;
         bit <- 0 until 63;
         word <- 0 until length) {

      util.Arrays.fill(data, 0L)
      data(word) = 1L << bit
      val bs = BitSet.fromBitMask(data.take(length))

      assertFalse(bs.isEmpty)
      assertEquals(1, bs.size)
      assertTrue(bs.hasDefiniteSize)
    }
  }
  def testForEach(data: List[Int]): Unit = {
    val testBuilder = BitSet.newBuilder
    testBuilder ++= data
    val underTest = testBuilder.result()

    val sbExpected = new StringBuilder()
    val sbActual   = new StringBuilder()
    data foreach { i => sbExpected append i.toString append " : " }
    underTest foreach { i => sbActual append i.toString append " : " }

    assertEquals(sbExpected.toString, sbActual.toString)
  }
  @Test def forEach: Unit = {
    datasets foreach testForEach
  }
  val sbExpected = new StringBuilder()
  val sbActual   = new StringBuilder()
  def testForAllImpl(data: List[Int], underTest: BitSet, p: Int => Boolean): Unit = {
    sbActual.clear
    sbExpected.clear

    assertEquals(s"data $data test $underTest p $p", data.forall(p), underTest.forall(p))
    assertEquals(s"data $data test $underTest p $p", sbExpected.toString, sbActual.toString)
  }
  def testExistsImpl(data: List[Int], underTest: BitSet, p: Int => Boolean): Unit = {
    sbActual.clear
    sbExpected.clear

    assertEquals(s"data $data test $underTest p $p", data.exists(p), underTest.exists(p))
    assertEquals(s"data $data test $underTest p $p", sbExpected.toString, sbActual.toString)
  }
  def testFindImpl(data: List[Int], underTest: BitSet, p: Int => Boolean): Unit = {
    sbActual.clear
    sbExpected.clear

    assertEquals(s"data $data test $underTest p $p", data.find(p), underTest.find(p))
    assertEquals(s"data $data test $underTest p $p", sbExpected.toString, sbActual.toString)
  }
  def testFilterImpl(data: List[Int], underTest: BitSet, p: Int => Boolean): Unit = {
    sbActual.clear
    sbExpected.clear

    assertEquals(s"data $data test $underTest p $p", data.filter(p).mkString("[", " : ", "]"), underTest.filter(p).mkString("[", " : ", "]"))
    assertEquals(s"data $data test $underTest p $p", sbExpected.toString, sbActual.toString)
  }
  def testFilterNotImpl(data: List[Int], underTest: BitSet, p: Int => Boolean): Unit = {
    sbActual.clear
    sbExpected.clear

    assertEquals(s"data $data test $underTest p $p", data.filterNot(p).mkString("[", " : ", "]"), underTest.filterNot(p).mkString("[", " : ", "]"))
    assertEquals(s"data $data test $underTest p $p", sbExpected.toString, sbActual.toString)
  }
  def testFor(data: List[Int], testImpl: (List[Int], BitSet, Int => Boolean) => Unit): Unit = {
    val testBuilder = BitSet.newBuilder
    testBuilder ++= data
    val underTest = testBuilder.result()

    testImpl(data, underTest, _ == -1)
    testImpl(data, underTest, _ >= 0)
    testImpl(data, underTest, _ > 0)
    testImpl(data, underTest, _ < 10000000)
    if (!data.isEmpty) {
      for (point <- List(data.head, data.last, data(data.size / 2))) {
        testImpl(data, underTest, _ < point)
        testImpl(data, underTest, _ <= point)
        testImpl(data, underTest, _ > point)
        testImpl(data, underTest, _ >= point)
        testImpl(data, underTest, _ == point)
        testImpl(data, underTest, _ != point)
      }
    }
    for (ds <- datasets) {
      val testBuilder = BitSet.newBuilder
      testBuilder ++= ds
      val filter = testBuilder.result()
      //as a set if a function, and there as special cases for them
      testImpl(data, underTest, filter)
    }
  }
  @Test def forAll: Unit = {
    datasets foreach (ds => testFor(ds, testForAllImpl _))
  }
  @Test def exists: Unit = {
    datasets foreach (ds => testFor(ds, testExistsImpl _))
  }
  @Test def find: Unit = {
    datasets foreach (ds => testFor(ds, testFindImpl _))
  }
  @Test def bulkAndSingleAddition: Unit = {
    for (ds1 <- datasets;
         ds2 <- datasets) {
      val expected = (ds1 ++ ds2).distinct.sorted.mkString("[", " : ", "]")

      val testBuilder1 = BitSet.newBuilder
      testBuilder1 ++= ds1
      var underTest = testBuilder1.result()

      val testBuilder2 = BitSet.newBuilder
      testBuilder2 ++= ds2
      val bs2 = testBuilder2.result()

      assertEquals(s"$underTest ... $ds2", expected, (underTest ++ ds2).mkString("[", " : ", "]"))
      assertEquals(s"$underTest ... $bs2", expected, (underTest ++ bs2).mkString("[", " : ", "]"))

      var control = (TreeSet.newBuilder[Int] ++= ds1).result
      assertEquals(s"$control ... $underTest",
                   control.mkString("[", " : ", "]"),
                   underTest.mkString("[", " : ", "]"))
      ds2 foreach {
        value =>
          control += value
          underTest += value
          assertEquals(s"$control ... $underTest  $value",
                       control.mkString("[", " : ", "]"),
                       underTest.mkString("[", " : ", "]"))

      }
    }
  }
  @Test def bulkAndSingleSubtraction: Unit = {
    for (ds1 <- datasets;
         ds2 <- datasets) {
      val expected = (ds1.toSet -- ds2).toList.sorted.mkString("[", " : ", "]")

      val testBuilder1 = BitSet.newBuilder
      testBuilder1 ++= ds1
      var underTest = testBuilder1.result()

      val testBuilder2 = BitSet.newBuilder
      testBuilder2 ++= ds2
      val bs2 = testBuilder2.result()

      assertEquals(expected, (underTest -- ds2).mkString("[", " : ", "]"))
      assertEquals(expected, (underTest -- bs2).mkString("[", " : ", "]"))

      var control = (TreeSet.newBuilder[Int] ++= ds1).result
      assertEquals(s"$control ... $underTest",
                   control.mkString("[", " : ", "]"),
                   underTest.mkString("[", " : ", "]"))
      ds2 foreach {
        value =>
          control -= value
          underTest -= value
          assertEquals(s"$control ... $underTest  $value",
                       control.mkString("[", " : ", "]"),
                       underTest.mkString("[", " : ", "]"))

      }
    }
  }
  @Test def union: Unit = {
    for (ds1 <- datasets;
         ds2 <- datasets) {
      val expected = (ds1.toSet union ds2.toSet).toList.sorted.mkString("[", " : ", "]")

      val testBuilder1 = BitSet.newBuilder
      testBuilder1 ++= ds1
      val underTest = testBuilder1.result()

      val testBuilder2 = BitSet.newBuilder
      testBuilder2 ++= ds2
      val bs2 = testBuilder2.result()

      assertEquals(expected, (underTest union ds2.toSet).mkString("[", " : ", "]"))
      assertEquals(expected, (underTest union bs2).mkString("[", " : ", "]"))

      assertEquals(expected, (underTest | bs2).mkString("[", " : ", "]"))
    }
  }
  @Test def intersect: Unit = {
    for (ds1 <- datasets;
         ds2 <- datasets) {
      val expected = (ds1.toSet intersect ds2.toSet).toList.sorted.mkString("[", " : ", "]")

      val testBuilder1 = BitSet.newBuilder
      testBuilder1 ++= ds1
      val underTest = testBuilder1.result()

      val testBuilder2 = BitSet.newBuilder
      testBuilder2 ++= ds2
      val bs2 = testBuilder2.result()

      assertEquals(expected, (underTest intersect ds2.toSet).mkString("[", " : ", "]"))
      assertEquals(expected, (underTest intersect bs2).mkString("[", " : ", "]"))

      assertEquals(expected, (underTest & bs2).mkString("[", " : ", "]"))
    }
  }
  @Test def diff: Unit = {
    for (ds1 <- datasets;
         ds2 <- datasets) {
      val expected = (ds1.toSet diff ds2.toSet).toList.sorted.mkString("[", " : ", "]")

      val testBuilder1 = BitSet.newBuilder
      testBuilder1 ++= ds1
      val underTest = testBuilder1.result()

      val testBuilder2 = BitSet.newBuilder
      testBuilder2 ++= ds2
      val bs2 = testBuilder2.result()

      assertEquals(expected, (underTest diff ds2.toSet).mkString("[", " : ", "]"))
      assertEquals(expected, (underTest diff bs2).mkString("[", " : ", "]"))

      assertEquals(expected, (underTest &~ bs2).mkString("[", " : ", "]"))
    }
  }
  @Test def filter: Unit = {
    datasets foreach (ds => testFor(ds, testFilterImpl _))
  }
  @Test def filterNot: Unit = {
    datasets foreach (ds => testFor(ds, testFilterNotImpl _))
  }
  @Test def testHashCode: Unit = {
    for (data <- datasets) {
      val testBuilder = BitSet.newBuilder
      testBuilder ++= data
      val underTest = testBuilder.result()

      val control = (TreeSet.newBuilder[Int] ++= data).result
      assertEquals(control.hashCode, underTest.hashCode)
    }
  }
  @Test def applyTrimmedToSize: Unit = {
    val l2 = (0 to 256 toList)
    val b2 = BitSet(l2: _*)

    assertEquals("List(ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, 1)",
                 b2.toBitMask.toList.map(_.toHexString).toString)
  }
  @Test def builderTrimmedToSize: Unit = {
    val l2 = (0 to 256 toList)
    val b2 = (BitSet.newBuilder ++= (l2) ).result

    assertEquals("List(ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, 1)",
                 b2.toBitMask.toList.map(_.toHexString).toString)
  }
}
