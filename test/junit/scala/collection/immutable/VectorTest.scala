package scala.collection.immutable

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}

@RunWith(classOf[JUnit4])
class VectorTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val v = Vector(0) ++ Vector(1 to 64: _*)

    assertEquals(Vector(0, 1), v take 2)
    assertEquals(Vector(63, 64), v takeRight 2)
    assertEquals(Vector(2 to 64: _*), v drop 2)
    assertEquals(Vector(0 to 62: _*), v dropRight 2)

    assertEquals(v, v take Int.MaxValue)
    assertEquals(v, v takeRight Int.MaxValue)
    assertEquals(Vector.empty[Int], v drop Int.MaxValue)
    assertEquals(Vector.empty[Int], v dropRight Int.MaxValue)

    assertEquals(Vector.empty[Int], v take Int.MinValue)
    assertEquals(Vector.empty[Int], v takeRight Int.MinValue)
    assertEquals(v, v drop Int.MinValue)
    assertEquals(v, v dropRight Int.MinValue)
  }

  @Test
  def hasCorrectPrependedAll(): Unit = {
    val els = Vector(1 to 1000: _*)

    for (i <- 0 until els.size) {
      val (prefix, suffix) = els.splitAt(i)

      assertEquals(els, prefix ++: suffix)
      assertEquals(els, prefix.toList ++: suffix)
    }
  }

  @Test
  def factoryReuse(): Unit = {
    assertSame(Vector.empty, Vector.empty)
    assertSame(Vector.empty, Vector())
    val m = Vector("a")
    assertSame(m, Vector.from(m))
    assertSame(m, Vector.apply(m: _*))
  }

  @Test def checkSearch: Unit = SeqTests.checkSearch(Vector(0 to 1000: _*), 15,  implicitly[Ordering[Int]])

  @Test
  def emptyIteratorReuse(): Unit = {
    assertSame(Vector.empty.iterator, Vector.empty.iterator)
    assertSame(Vector.empty.iterator, Vector(1).drop(1).iterator)
  }

  @Test
  def t11122_prependedAll_Iterator(): Unit = {
    val i = Iterator.from(1).take(3)
    assertEquals(Vector(1, 2, 3, 0), Vector(0).prependedAll(i))
  }

  @Test
  def concat: Unit = {
    assertEquals((1 to 100).toVector, (1 to 7).toVector concat (8 to 100).toVector)
  }

  @Test
  def copyToArray: Unit = {
    val array = Array.fill(100)(2)
    Vector.fill(100)(1).copyToArray(array, 0, 100)
    assertEquals(array.toSeq, Seq.fill(100)(1))
  }

  @Test
  def vectorIteratorDrop(): Unit = {
    val underlying = Vector(0 to 10010: _*)

    val totalSize = underlying.size

    for (start <- 1056 to 10000) {
      val it = underlying.iterator.drop(start)
      assertEquals(totalSize - start, it.knownSize)
      assertEquals(totalSize - start, it.size)
      assertTrue(it.hasNext)
      assertEquals(start, it.next())
    }
  }
  def intercept[T <: Throwable: ClassTag](fn: => Any): T = {
    try {
      fn
      fail(s"expected a ${classTag[T].runtimeClass.getName} to be thrown")
      ???
    } catch {
      case x: T => x
    }
  }
  @Test
  def vectorIteratorDropToEnd(): Unit = {
    val underlying = Vector(0)

    for (start <- List(1,2,3,4,99)) {
      {
        var it = underlying.iterator.drop(start)
        assertFalse(it.hasNext)
        intercept[NoSuchElementException](it.next)
        it = it.drop(0)
        assertFalse(it.hasNext)
        it = it.drop(1)
        assertFalse(it.hasNext)
        it = it.drop(99)
        assertFalse(it.hasNext)
        intercept[NoSuchElementException](it.next)
      }

      {
        var it = underlying.iterator.drop(start)
        intercept[NoSuchElementException](it.next)
        it = it.drop(0)
        it = it.drop(1)
        it = it.drop(99)
        intercept[NoSuchElementException](it.next)
      }
    }
  }
  @Test
  def vectorIteratorRepeated(): Unit = {
    val underlying = Vector(1 to 10001: _*)


    for (stepSize <- List(0, 1, 2, 3, 4, 8, 10, 24, 32, 63, 64, 100)) {
      var it:Iterator[Int] = underlying.iterator
      for (stepCount <- 1 to 10) {
        it = it.drop(stepSize)
        assertTrue(it.hasNext)
        val expected = (stepSize + 1) * stepCount
        assertEquals(expected, it.next())
      }
    }
  }
  @Test
  def vectorFill(): Unit = {
    var i = 0
    val test = Vector.fill(10){
      i += 1
      i * 10
    }
    assertEquals(List(10,20,30,40,50,60,70,80,90,100), test)
    assertEquals(10, test.length)
    assertEquals(10, test.head)
    assertEquals(10, test(0))
    assertEquals(20, test(1))
    assertEquals(80, test(7))
    assertEquals(100, test(9))

    assertEquals(0, test.indexOf(10))
    assertEquals(8, test.indexOf(90))
    assertEquals(-1, test.indexOf(1000))
  }

  @Test
  def tapEach(): Unit = {
    val lb = ListBuffer[Int]()

    val v =
      Vector(1,2,3)
      .tapEach(lb += _)
      .tapEach(lb += _)

    assertEquals(ListBuffer(1,2,3,1,2,3), lb)
    assertEquals(Vector(1,2,3), v)


    val f: Any => Unit = println

    // test that type info is not lost
    val x: Vector[Char] = Vector[Char]().tapEach(f)
  }

  @Test
  def vectorIteratorTake(): Unit = {
    val v = Vector.from(0 to 50)
    for {
      i <- -100 to 4000 by 40
      j <- -100 to 4000 by 6
    } {
      val v2 = v.take(i)
      assertArrayEquals(s"<${v2.length}>.take($j)", v2.toArray.take(j), v2.iterator.take(j).toArray)
    }
  }

  @Test
  def vectorIteratorDrop2(): Unit = {
    val v = Vector.from(0 to 50)
    for {
      i <- -100 to 4000 by 40
      j <- -100 to 4000 by 60
    } {
      val v2 = v.take(i)
      assertArrayEquals(s"<${v2.length}>.drop($j)", v2.toArray.drop(j), v2.iterator.drop(j).toArray)
    }
  }

  @Test
  def vectorIteratorSlice(): Unit = {
    val v = Vector.from(0 to 50)
    for {
      i <- -100 to 4000 by 40
      j <- -100 to 4000 by 60
      k <- -100 to 4000 by 60
    } {
      val v2 = v.take(i)
      assertArrayEquals(s"<${v2.length}>.slice($j, $k)", v2.toArray.slice(j, k), v2.iterator.slice(j, k).toArray)
    }
  }
  @Test
  def t11600(): Unit = {
    locally {
      abstract class Base
      class Derived1 extends Base
      class Derived2 extends Base
      val d1 = new Derived1
      val d2 = new Derived2

      locally {
        val arraySeq = ArraySeq(d1)
        val vector = Vector(arraySeq: _*)
        assertEquals(arraySeq, ArraySeq(d1)) // ensure arraySeq is not mutated
        assertEquals(vector.updated(0, d2), Vector(d2))
      }

      locally {
        val list = List(d1)
        val vector = Vector.from(list)
        assertEquals(list, vector)
        assertEquals(List(d2), vector.updated(0, d2))
      }
    }

    locally {
      // ensure boxing logic works:
      val arraySeq = ArraySeq(1,2,3,4,5)
      val vector = Vector(arraySeq: _*)

      assertEquals(1 to 5, vector)
      assertEquals(vector.updated(0, 20), Vector(20,2,3,4,5))
      assertEquals(vector.updated(0, ""), Vector("",2,3,4,5))
      assertEquals(1 to 5, arraySeq) // ensure arraySeq is not mutated
    }
    locally {
      // ensure boxing logic works:
      val arr = Array(1)
      val vector = Vector.from(arr)
      assertEquals(arr.toList, vector)
      assertEquals(List(20), vector.updated(0, 20))
      assertEquals(List(""), vector.updated(0, ""))
    }
  }
}
