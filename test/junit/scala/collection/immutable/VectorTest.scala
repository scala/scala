package scala.collection.immutable

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.annotation.{nowarn, unused}
import scala.collection.immutable.VectorInline.{WIDTH3, WIDTH4, WIDTH5}
import scala.collection.mutable.{ListBuffer, StringBuilder}
import scala.tools.testkit.AssertUtil.intercept

@RunWith(classOf[JUnit4])
class VectorTest {
  import VectorUtils.validateDebug

  @Test
  def t12564(): Unit = {
    val vector1 = Vector.fill(1000)(0)
    // Some prepending, manifesting the bug seems to require 2 separate prepends
    val vector2 = List.fill(25)(1) ++: 2 +: vector1
    // Now append for fun and profit
    val vector3 = vector2 ++ List.fill(40)(3)
    // Any iteration will do although might hit NullPointerException in different place
    val vector4 = vector3.collect { case n => n }
    assert(vector3 == vector4)
  }

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
  def hasCorrectAppendedAndPrependedAll(): Unit = {
    val els = Vector(1 to 1200: _*)

    for (i <- 0 until els.size) {
      val (prefix, suffix) = els.splitAt(i)
      validateDebug(prefix)
      validateDebug(suffix)
      validateDebug(prefix ++: suffix)
      assertEquals((prefix, suffix).toString, els, prefix ++: suffix)
      assertEquals((prefix, suffix).toString, els, prefix :++ suffix)
      assertEquals(els, prefix.toList ++: suffix)
      assertEquals(els, prefix.toList :++ suffix)
    }
  }

  @Test
  def testBuilderAddVector(): Unit = {
    import VectorInline._
    val b = new VectorBuilder[Int]
    val expected1 = Vector.from(1 to 32).asInstanceOf[Vector1[Int]]
    b.initFrom(expected1)
    val expected2 = Vector.from(33 to 128).asInstanceOf[Vector2[Int]]
    b.addAll(expected2) // uses addVector with Vector2, aligned
    b.addOne(129)
    val expected3 = Vector.from(130 to 224).asInstanceOf[Vector2[Int]]
    b.addAll(expected3) // uses addVector with Vector2, but misaligned
    b.addAll(Vector.from(225 to 4096)) // uses addVector with Vector3, aligned to 32, but not 1024
    b.addAll(Vector.from(4097 to 8192)) // uses addVector with Vector3, aligned to 1024
    b.addOne(8193) // builder still working for single element?
    b.addAll(Vector.from(8193 to 234567).tail) // aligned to 1024, split at arbitrary number 234567
    b.addAll(Vector.from(1 to 42 * WIDTH3).drop(234567)) // aligned to pow(32,3)
    val res = b.result().asInstanceOf[Vector5[Int]]
    assertEquals(1 to 42 * WIDTH3, res)

    assertSame("b.initFrom did not keep original array but copied instead", expected1.prefix1, res.prefix1)

//    assertSame(s"b.addVector did not keep original array but copied instead (${expected2.prefix1.head},...,${expected2.prefix1.last} vs ${res.prefix2(0).head},...,${res.prefix2(0).last}).", expected2.prefix1, res.prefix2(0)) // prefix1 is not reused, as addArr1 is called
    assertSame("b.addVector did not keep original array but copied instead", expected2.data2(0), res.prefix2(1)) // expected2's arrays are reused

    assertTrue("", expected3.suffix1.length != 32) // expected3 is misaligned
  }

  @Test
  def testBuilderAlignTo1(): Unit = {
    // v3 == Vector(0, 1, ..., 1999), but alignment is no multiple of 32 or 1024
    val v3 = Vector.tabulate(2042)(i => (i - 42).toString).drop(42).asInstanceOf[Vector3[AnyRef]]
    for (i <- Seq(0, 5, 123, 949, 950, 982, 1024, 1999, 2000)) {
      val (a, b) = v3.splitAt(i)
      val res = new VectorBuilder[AnyRef]
        .alignTo(i, b)
        .addAll(a.toList) // ensure there is no alignment in a
        .addAll(b)
        .result()
        .asInstanceOf[Vector3[AnyRef]]
      assertEquals(s"values equal when split at $i", v3, res)
      if (i < 950) // (v3.prefix1++v3.prefix2).size == 982. So when dropping >= 950 elements, and keeping prefix1 nonempty (=> has 32 elements), prefix2 would be empty. Instead, suffix2's content is stored in prefix2, so the alignment (len12) changes and it's okay.
        assertEquals(s"alignment is the same when split at $i", v3.len12, res.len12)
    }
  }

  @Test
  def testBuilderAlignTo2(): Unit = {
    val Large = 1 << 20
    @nowarn val KrazyKonstant = (1 << 31) - (1 << 26) - 1000
    for {
      size <- Seq(0, 1, 31, 1 << 5, 1 << 10, 1 << 15, 1 << 20, 9 << 20, 1 << 25, 9 << 25, 50 << 25, 1 << 30, KrazyKonstant)
      i <- Seq(0, 1, 5, 123)
    } {
//      println((i, size))
      val v = if (size < Large) Vector.tabulate(size)(_.toString) else Vector.fillSparse(size)("v")
      val prefix = Vector.fill(i)("prefix")
      val vb = new VectorBuilder[AnyRef]
        .alignTo(i, v)
        .addAll(prefix)
        .addAll(v)
      val res = vb.result()
      val vDesc = if (v.headOption.contains("v")) s"Vector(\"v\")*$size" else s"Vector(0,..,${size-1})"
      assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc).size", size + i, res.size)
      assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc).take($i)", prefix, res.take(i))
      assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc).drop($i)", v.take(Large), res.drop(i).take(Large))

      if (size == 9 << 20) {
        val v4     = Vector.fillSparse(WIDTH3 * 2)("v4")
        val suffix = (v4 ++ v).take(size)
        val res2   = vb.addAll(suffix).result()
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).size", 2 * size + i, res2.size)
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).take($i)", prefix, res2.take(i))
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).drop($i).take($size)", v.take(Large), res2.drop(i).take(Large))
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).drop(${i + size}).take($size)", suffix.take(Large), res2.drop(i + size).take(Large))
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).drop(${i + 2 * size})", suffix.drop(size).take(Large), res2.drop(i + 2 * size).take(Large))
      } else if (size == 9 << 25) {
        val v4     = Vector.fillSparse(WIDTH4 * 2)("v4")
        val suffix = (v4 ++ v).take(size)
        val res2   = vb.addAll(suffix).result()
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).size", 2 * size + i, res2.size)
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).take($i)", prefix, res2.take(i))
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).drop($i).take($size)", v.take(Large), res2.drop(i).take(Large))
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).drop(${i + size}).take($size)", suffix.take(Large), res2.drop(i + size).take(Large))
        assertEquals(s"(Vector(\"prefix\")*$i ++ $vDesc ++ v4 ++ $vDesc).drop(${i + 2 * size})", suffix.drop(size).take(Large), res2.drop(i + 2 * size).take(Large))
      } else if (size == 50 << 25) {
        assertThrows(classOf[IllegalArgumentException], () => vb.addAll(v))
      }
    }
  }

  @Test
  def testBuilderInitWithLargeVector(): Unit = {
    val v    = Vector.fillSparse(Int.MaxValue / 4 * 3)("v")
    val copy =
      new VectorBuilder[String]
        .initFrom(v)
        .result()
    assertEquals(copy.size, v.size)
    assertEquals(copy.take(500), v.take(500))
  }

  @Test
  def testWeirdAlignments1(): Unit = {
    val v3 = Vector.tabulate(2042)(i => (i - 42).toString).drop(42).asInstanceOf[Vector3[AnyRef]]
    for (i <- Seq(0, 1, 5, 41, 42, 43, 123, 949, 950, 982, 1024, 1999, 2000)) {
      val res = new VectorBuilder[AnyRef]
        .alignTo(i, v3) // pretend to add i elements before v3, but then decide not to... this is slow, but should not fail
        .addAll(v3)
        .result()
        .asInstanceOf[Vector3[AnyRef]]
      assertEquals(s"vectors equal", v3, res)
    }
  }

  @Test
  def testWeirdAlignments2(): Unit = {
    val v3 = Vector.tabulate(2042)(i => (i - 42).toString).drop(42).asInstanceOf[Vector3[AnyRef]]
    val v2 = Vector.tabulate(765)(i => (i - 123).toString).drop(123).asInstanceOf[Vector2[AnyRef]]
    for (i <- Seq(-1234, -42, -1, 0, 1, 5, 41, 42, 43, 123, 949, 950, 982, 1024, 1999, 2000)) {
      val res = new VectorBuilder[AnyRef]
        .alignTo(i, v3) // pretend to add v3 ...
        .addOne("a")
        .addAll(v2) // ... but then add completely unrelated v2 instead!
        .result()
      assertEquals(s"vectors equal", "a" +: v2, res)
    }
  }

  @Test
  def testWeirdAlignments3(): Unit = for (n <- allSizes; m <- verySmallSizes) {
    val vPretend =
      if (smallSizes.contains(n))
        Vector.tabulate(n + 1337)(i => (i - 1337).toString).drop(1337)
      else
        Vector.fillSparse(n + 1337)("v").drop(1337)
    val vReal    = Vector.tabulate(m + 42)(i => (i - 42).toString).drop(42)
    for (i <- Seq(-1234, -42, -1, 0, 1, 5, 41, 42, 43, 123, 949, 950, 982, 1024, 1999, 2000, 1234567)) {
      val vb   = new VectorBuilder[AnyRef]
        .alignTo(i, vPretend) // pretend to add vPretend ...
        .addAll(vReal) // ... but then add completely unrelated vReal instead!
      val res1 = vb.result()
      assertEquals(s"vectors not equal, n=$n, m=$m, i=$i", vReal, res1)
      val res2 = vb
        .addOne("a")
        .addAll(vReal) // reuse the builder
        .result()
      assertEquals(s"vectors not equal, n=$n, m=$m, i=$i", (vReal :+ "a") ++ vReal, res2)
    }
  }

  @Test
  def testWeirdAlignments4(): Unit = {
    var lengths = Set[Int]()
    for (
      n <- allSizes.init :+ (allSizes.last - WIDTH5 - WIDTH3 + 41); // we need WIDTH5 for prefix, add 1+WIDTH3 and get 42 in suffix free
      m <- List(WIDTH4, WIDTH5, Int.MaxValue - WIDTH5)
    ) {
      val vPretend = Vector.fillSparse(42)("v0") ++ Vector.fillSparse(m - 42)("v1")
      val vReal    = vPretend.take(n)
      //    if (n==1073741824 && m==2046820352)
      //      println(s"n=$n, m=$m")
      val vb = new VectorBuilder[AnyRef]
        .alignTo(0, vPretend) // pretend to add vPretend ...
        .addAll(vReal) // ... but then add only a subsequence!
      val res1 = vb.result()
      assertEquals(s"vectors not equal, n=$n, m=$m", vReal.length, res1.length)
      assertEquals(s"vectors not equal, n=$n, m=$m", vReal.take(WIDTH3), res1.take(WIDTH3))
      assertEquals(s"vectors not equal, n=$n, m=$m", vReal.takeRight(WIDTH3), res1.takeRight(WIDTH3))
      val res2 = vb
        .addOne("a")
        .addAll(vReal.take(WIDTH3)) // whole vector may take too long
        .result()
      val expected = (vReal :+ "a") ++ (vReal.take(WIDTH3))
      assertEquals(s"vectors not equal, n=$n, m=$m", expected.length, res2.length)
      assertEquals(s"vectors not equal, n=$n, m=$m", expected.take(WIDTH3), res2.take(WIDTH3))
      assertEquals(s"vectors not equal, n=$n, m=$m", expected.takeRight(WIDTH3), res2.takeRight(WIDTH3))
      lengths += res2.length
    }
    assertEquals(15, lengths.size)
    assertEquals(Int.MaxValue - WIDTH5 + 42, lengths.max)
  }

  @Test
  def testNegativeAlignment(): Unit = for (size <- allSizes; i <- allSizes) {
    val v        = Vector.fillSparse(math.min(63 * WIDTH5, size))("v")
    val expected = v.drop(i)
    val vb       = new VectorBuilder[AnyRef]
      .alignTo(-i, v)
      .addAll(expected)
    val res      = vb.result()
    assertEquals("lengths not equal", expected.length, res.length)
    assertEquals(s"vectors not equal", expected.take(WIDTH3), res.take(WIDTH3))
    assertEquals(s"vectors not equal", expected.takeRight(WIDTH3), res.takeRight(WIDTH3))
  }

  @Test
  def factoryReuse(): Unit = {
    assertSame(Vector.empty, Vector.empty)
    assertSame(Vector.empty, Vector())
    val m = Vector("a")
    assertSame(m, Vector.from(m))
    assertSame(m, Vector.apply(m: _*))
  }

  @Test def factoryReuseArraySet(): Unit = {
    val arraySeq = ArraySeq[AnyRef]("a", "b")
    val vectorFromArraySeq = Vector.from(arraySeq)
    val prefix1Field = classOf[Vector[_]].getDeclaredField("prefix1")
    prefix1Field.setAccessible(true)
    assertSame(arraySeq.unsafeArray, prefix1Field.get(vectorFromArraySeq))
  }

  @Test def checkSearch(): Unit = SeqTests.checkSearch(Vector(0 to 1000: _*), 15,  implicitly[Ordering[Int]])

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
  def concat(): Unit = {
    assertEquals(Vector.from(1 to 100), Vector.from(1 to 7) concat Vector.from(8 to 100))
  }

  @Test
  def copyToArray(): Unit = {
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

  @Test
  def vectorIteratorDropToEnd(): Unit = {
    val underlying = Vector(0)

    for (start <- List(1,2,3,4,99)) {
      {
        var it = underlying.iterator.drop(start)
        assertFalse(it.hasNext)
        intercept[NoSuchElementException](it.next())
        it = it.drop(0)
        assertFalse(it.hasNext)
        it = it.drop(1)
        assertFalse(it.hasNext)
        it = it.drop(99)
        assertFalse(it.hasNext)
        intercept[NoSuchElementException](it.next())
      }

      {
        var it = underlying.iterator.drop(start)
        intercept[NoSuchElementException](it.next())
        it = it.drop(0)
        it = it.drop(1)
        it = it.drop(99)
        intercept[NoSuchElementException](it.next())
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


    val f: Any => Unit = _ => ()

    // test that type info is not lost
    @unused val x: Vector[Char] = Vector[Char]().tapEach(f)
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

  def t11636(): Unit = {
    val a: Vector[String] = "O" +: Iterator.continually("E").take(2101).foldLeft(Vector.empty[String])((v, e) => v :+ e) :+ "C"
    val a0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(2101) ++ ArraySeq("C")

    val b: Vector[String] = "O" +: Iterator.continually("E").take(223) .foldLeft(Vector.empty[String])((v, e) => v :+ e) :+ "C"
    val b0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(223) ++ ArraySeq("C")

    val c: Vector[String] = "O" +: Iterator.continually("E").take(135) .foldLeft(Vector.empty[String])((v, e) => v :+ e) :+ "C"
    val c0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(135) ++ ArraySeq("C")

    val d: Vector[String] = "O" +: Iterator.continually("E").take(0)   .foldLeft(Vector.empty[String])((v, e) => v :+ e) :+ "C"
    val d0: ArraySeq[String] = ArraySeq("O", "C")

    val e: Vector[String] = "O" +: Iterator.continually("E").take(376) .foldLeft(Vector.empty[String])((v, e) => v :+ e) :+ "C"
    val e0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(376) ++ ArraySeq("C")

    val f: Vector[String] = "O" +: Iterator.continually("E").take(365) .foldLeft(Vector.empty[String])((v, e) => v :+ e) :+ "C"
    val f0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(365) ++ ArraySeq("C")

    assertEquals(a0 ++ b0, a ++ b)
    assertEquals(a0 ++ b0 ++ c0, a ++ b ++ c)
    assertEquals(a0 ++ b0 ++ c0 ++ d0, a ++ b ++ c ++ d)
    assertEquals(a0 ++ b0 ++ c0 ++ d0 ++ e0, a ++ b ++ c ++ d ++ e)
    assertEquals(a0 ++ b0 ++ c0 ++ d0 ++ e0 ++ f0, a ++ b ++ c ++ d ++ e ++ f)
  }

  import VectorInline.WIDTH
  val allSizes = Seq(0, 1, WIDTH, WIDTH+1, WIDTH*WIDTH, WIDTH*WIDTH+1, WIDTH*WIDTH*WIDTH, WIDTH*WIDTH*WIDTH+1,
    WIDTH*WIDTH*WIDTH*WIDTH, WIDTH*WIDTH*WIDTH*WIDTH+1, WIDTH*WIDTH*WIDTH*WIDTH*WIDTH, WIDTH*WIDTH*WIDTH*WIDTH*WIDTH+1,
    WIDTH*WIDTH*WIDTH*WIDTH*WIDTH*WIDTH, WIDTH*WIDTH*WIDTH*WIDTH*WIDTH*WIDTH+1, Int.MaxValue)
  val smallSizes = allSizes.filter(_ <= WIDTH*WIDTH*WIDTH*WIDTH)
  val verySmallSizes = allSizes.filter(_ <= WIDTH*WIDTH)

  @Test
  def testAligned(): Unit = for(size <- smallSizes) {
    val v = Vector.range(0, size)
    //println(v.toDebugString)
    validateDebug(v)
    assertEquals(size, v.length)
    assertEquals(0 until size, v)
  }

  @Test
  def testNonAligned(): Unit = for(size <- smallSizes.filter(n => n > 0 && n < WIDTH*WIDTH*WIDTH*WIDTH)) {
    val v0 = Vector.range(1, size)
    val v = 0 +: v0
    //println(v0.toDebugString)
    //println(v.toDebugString)
    validateDebug(v)
    assertEquals(size, v.length)
    assertEquals(0 until size, v)
  }


  @Test
  def testUpdate(): Unit = for(size <- smallSizes) {
    var v = Vector.range(0, size)
    var i = 0
    while(i < size) {
      v = v.updated(i, i-13)
      i += 1
    }
    assertEquals(-13 until size-13, v)
  }

  @Test
  def testSlice1(): Unit = for(size <- smallSizes) {
    val step = size/16 max 1
    val v = Vector.range(0, size)
    for {
      from <- 0 until size by step
      to <- from until size by step
    } {
      //println(s"-------------------------- size=$size, from=$from, to=$to")
      //println("v: "+VectorUtils.toDebugString(v))
      val v2 = v.slice(from, to)
      //println("v2: "+VectorUtils.toDebugString(v2))
      validateDebug(v2)
      assertEquals(from until to, v2)
    }
  }

  @Test
  def testSlice2(): Unit = for(size <- Seq(10, 100, 1000, 10000)) {
    val o = new AnyRef
    val coll = Vector.fill(size)(o)
    val inc = size / 10
    if(inc > 0) {
      var i = 0
      while(i < size) {
        var j = i + inc
        while(j < size) {
          //println(s"--- $i, $j")
          //println(VectorUtils.toDebugString(coll))
          coll.slice(i, j)
          j += inc
        }
        i += inc
      }
    }
  }

  @Test def `test slice to MinValue`: Unit = {
    assertTrue(Vector(42).slice(1, Int.MinValue).isEmpty)
    assertTrue("slice almost max to min should be empty", Vector(42).slice(Int.MaxValue-1, Int.MinValue).isEmpty)
    assertTrue("slice max to min should be empty", Vector(42).slice(Int.MaxValue, Int.MinValue).isEmpty)
  }

  @Test def `test Vector#iterator slice to MinValue`: Unit = {
    assertTrue(Vector(1, 2).iterator.slice(1, Int.MinValue).isEmpty)
    assertTrue("slice almost max to min should be empty", Vector(1, 2).iterator.slice(Int.MaxValue - 1, Int.MinValue).isEmpty)
    assertTrue("slice max to min should be empty", Vector(1, 2).iterator.slice(Int.MaxValue, Int.MinValue).isEmpty)
  }

  @Test
  def testTail(): Unit = for(size <- verySmallSizes) {
    var i = 0
    var v = Vector.range(0, size)
    //println("testTail: "+size)
    while(!v.isEmpty) {
      v = v.tail
      i += 1
      assertEquals(i until size, v)
    }
  }

  @Test
  def testRebuild(): Unit = for(size <- smallSizes) {
    for(prependSize <- verySmallSizes) {
      var v = Vector.range(0, size + prependSize)
      for(i <- prependSize to 0 by -1) {
        v = i +: v
      }
      validateDebug(v)
      val b = Vector.newBuilder[Int]
      b.addAll(v)
      val v2 = b.result()
      validateDebug(v2)
      assertEquals(v, v2)
    }
  }

  @Test
  def testAppendAll(): Unit = for(size <- smallSizes) {
    for(appendSize <- smallSizes) {
      val v = Vector.range(0, size)
      val v2 = Vector.range(size, size + appendSize)
      val v3 = v.appendedAll(v2)
      assertEquals(0 until (size + appendSize), v3)
    }
  }

  // scala/bug#12027
  // This took forever in 2.13.2
  @Test def testAppendWithTinyLhs(): Unit = {
    (1 to 1000000).foldLeft(Vector.empty[Int]) { (v, i) => Vector(i) ++ v }
    (1 to 1000000).foldLeft(Vector.empty[Int]) { (v, i) => v.prependedAll(Vector(i)) }
  }

  @Test def `t12598 bad index on prependedAll`: Unit = {
    val v = Vector.from(1 to 10000)
    val (pre, suf) = v.splitAt(1)
    val v2 = suf.prependedAll(pre)
    validateDebug(v2)
    // Validation failed: assertion failed: sum of slice lengths (1024) at prefix2 should be equal to prefix length (1023)
    // [error] Vector3(lengths=[32, 1023, 9215, 9984, 10000])
    assertEquals(10000, v.last)
    assertEquals(10000, v2(v2.length-1)) // ArrayIndexOutOfBoundsException
    assertEquals(1024, v2(1023))
  }
  @Test def `t12598b bad index on prependedAll`: Unit = {
    val v = Vector.from(1 to 1000000)
    val (pre, suf) = v.splitAt(1)
    val v2 = suf.prependedAll(pre)
    validateDebug(v2)
    // Validation failed: assertion failed: sum of slice lengths (1024) at prefix2 should be equal to prefix length (1023)
    // [error] Vector3(lengths=[32, 1023, 9215, 9984, 10000])
    assertEquals(1000000, v.last)
    assertEquals(1000000, v2(v2.length-1)) // ArrayIndexOutOfBoundsException
    assertEquals(1024, v2(1023))
  }
}

object VectorUtils {
  import VectorInline._

  def validateDebug(v: Vector[_]): Unit = {
    try validate(v) catch {
      case ex: Throwable =>
        throw new RuntimeException("Validation failed: " + ex.getMessage + "\n" + toDebugString(v), ex)
    }
  }

  def validate(v: Vector[_]): Unit = {
    val count = v.vectorSliceCount
    val len = (0 until count).map(v.vectorSlicePrefixLength)
    val alen = (0 until count).map { i =>
      if(i == 0 || i == (count-1)) v.vectorSlice(i).length
      else validateArrays(v.vectorSlice(i).asInstanceOf[Array[Array[AnyRef]]], sliceName(v, i))
    }
    val running = alen.scanLeft(0)(_ + _).tail
    running.zip(len).zipWithIndex.foreach { case ((r, p), i) =>
      assert(r == p, s"sum of slice lengths ($r) at ${sliceName(v, i)} should be equal to prefix length ($p)")
    }
  }

  def sliceName(v: Vector[_], i: Int): String = {
    val count = v.vectorSliceCount
    val level = (count+1)/2
    if(i+1 == level) "data"
    else if(i+1 < level) "prefix" + (i+1)
    else "suffix" + (count-i)
  }

  def toDebugString(v: Vector[_]): String = {
    val sb = new StringBuilder()
    val level = (v.vectorSliceCount+1)/2
    val len = (0 until v.vectorSliceCount).map(v.vectorSlicePrefixLength)
    sb.append(s"Vector$level(lengths=[${len.mkString(", ")}])\n")
    for(i <- 0 until v.vectorSliceCount)
      logArray(sb, v.vectorSlice(i), prefix = s"${sliceName(v, i)}: ")
    sb.result()
  }

  def toDebugString(v: VectorSliceBuilder): String = {
    val sb = new StringBuilder()
    sb.append(s"$v\n")
    logArray(sb, v.getSlices, prefix = "slices: ")
    sb.result()
  }

  @unused private def toDebugString(v: VectorBuilder[_]): String = {
    val sb = new StringBuilder()
    sb.append(s"$v\n")
    val d = v.getData.asInstanceOf[Array[Array[AnyRef]]]
    logArray(sb, d(5), prefix = "a6: ")
    logArray(sb, d(4), "a5: ", "  ", d(5).asInstanceOf[Array[Array[AnyRef]]], "a6")
    logArray(sb, d(3), "a4: ", "  ", d(4).asInstanceOf[Array[Array[AnyRef]]], "a5")
    logArray(sb, d(2), "a3: ", "  ", d(3).asInstanceOf[Array[Array[AnyRef]]], "a4")
    logArray(sb, d(1), "a2: ", "  ", d(2).asInstanceOf[Array[Array[AnyRef]]], "a3")
    logArray(sb, d(0), "a1: ", "  ", d(1).asInstanceOf[Array[Array[AnyRef]]], "a2")
    sb.result()
  }

  private[this] def validateArrays[T](a: Array[Array[T]], name: String): Int = {
    var i = 0
    var total = 0
    while(i < a.length) {
      assert(a(i) ne null, s"$name($i) should not be null")
      assert(a(i).length == WIDTH, s"$name($i) should have length $WIDTH, has ${a(i).length}")
      if(a(i).isInstanceOf[Array[Array[AnyRef]]])
        total += validateArrays(a(i).asInstanceOf[Array[Array[AnyRef]]], s"$name($i)")
      else if(a(i).isInstanceOf[Array[AnyRef]])
        total += a(i).asInstanceOf[Array[AnyRef]].length
      i += 1
    }
    total
  }

  private[this] def logArray[T <: AnyRef](sb: StringBuilder, a: Array[T], prefix: String, indent: String = "  ", findIn: Array[Array[T]] = null, findInName: String = "<array>"): StringBuilder = {
    def classifier(x: AnyRef): String =
      if(x eq null) "-"
      else if(x.isInstanceOf[Array[AnyRef]]) "A"
      else if((x: Any).isInstanceOf[Int]) x.toString
      else "o"
    def atos(a: Array[_ <: AnyRef]): String =
      if(a eq null) "-"
      else {
        var i = 0
        var startNum: Option[Int] = None
        var currentNum = 0
        val b = new StringBuilder().append("[")
        while(i < a.length) {
          if(i != 0) b.append(",")
          (a(i): Any) match {
            case n: Int =>
              if(i == 0) {
                startNum = Some(n)
                currentNum = n
              } else if(startNum.isDefined) {
                if(n == currentNum +1) currentNum = n
                else startNum = None
              }
            case _ => startNum = None
          }
          b.append(classifier(a(i)))
          i += 1
        }
        if(startNum.isDefined && startNum.get != currentNum) {
          b.clear()
          b.append("[").append(startNum.get).append("...").append(currentNum)
        }
        b.append("]").toString + " (" + a.length + ")"
      }
    if(a eq null)
      sb.append(indent + prefix + "-\n")
    else {
      val idx = Option(findIn).map(_.indexWhere(_ eq a)).getOrElse(-1)
      if(idx >= 0) {
        sb.append(s"$indent$prefix= $findInName($idx)\n")
      } else {
        sb.append(indent + prefix + atos(a) + "\n")
        var i = 0
        while(i < a.length) {
          if(a(i).isInstanceOf[Array[AnyRef]]) {
            logArray(sb, a(i).asInstanceOf[Array[AnyRef]], s"$i. ", indent = indent + "  ", null, null)
          }
          i += 1
        }
      }
    }
    sb
  }
}
