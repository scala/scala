package scala.collection

import org.junit.{Assert, Test}
import Assert.{assertEquals, assertFalse, assertTrue}

import scala.annotation.nowarn
import scala.collection.immutable.{ArraySeq, List, Range, Vector}
import scala.tools.testkit.AssertUtil._

class IterableTest {

  def f(xs: Seq[Seq[Int]], ys: Seq[Int]): Unit = {
    assert(xs.flatten == ys)
    assert(ys.flatMap(y => Some(y)) == ys.map(y => Some(y)).flatten)
  }

  @Test
  def flattenTest(): Unit = {
    f(List(ArraySeq(1, 2, 3)), List(1, 2, 3))
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def concatTest(): Unit = {
    val seq = Seq.concat(Seq(1, 2, 3), Iterable(4, 5, 6))
    assert(seq == Seq(1, 2, 3, 4, 5, 6))

    val vector = Vector.concat(Seq(1, 2, 3), Iterable(4, 5, 6))
    assert(vector == Vector(1, 2, 3, 4, 5, 6))

    val set = Set.concat(Seq(1, 2, 3), Iterable(3, 4, 5))
    assert(set == Set(1, 2, 3, 4, 5))

    val iterator = Iterator.concat(Seq(1, 2, 3), Seq(4, 5, 6))
    assert(iterator.toSeq == Seq(1, 2, 3, 4, 5, 6))
    assert(iterator.isEmpty)

    assertEquals(Seq(1,2,3,4,5,6), Seq(1,2,3).concat(Seq(4,5,6)))
    assertEquals(Seq(1,2,3,4,5,6), Seq(1,2,3).concat(Iterator(4,5,6)))
    assertEquals(Seq(1,2,3,4,5,6), Seq.from(Iterator(1,2,3).concat(Iterator(4,5,6))))

    assertEquals(Iterable(1,2,3), Iterable(1) ++: Iterable(2,3))
    assertEquals(Iterable(1,2,3), Iterator(1) ++: Iterable(2,3))
  }

  @Test
  def groupMap(): Unit = {
    case class User(name: String, age: Int)

    def namesByAge(users: Set[User]): Map[Int, Set[String]] =
      users.groupMap(_.age)(_.name)

    val users =
      Set(User("Alice", 12), User("Bob", 14), User("Charlie", 12))
    val expected = Map(12 -> Set("Alice", "Charlie"), 14 -> Set("Bob"))
    assertEquals(expected, namesByAge(users))
  }

  @Test
  def groupMapReduce(): Unit = {
    def occurrences[A](as: Seq[A]): Map[A, Int] =
      as.groupMapReduce(identity)(_ => 1)(_ + _)

    val xs = Seq('a', 'b', 'b', 'c', 'a', 'a', 'a', 'b')
    val expected = Map('a' -> 4, 'b' -> 3, 'c' -> 1)
    assertEquals(expected, occurrences(xs))
  }

  @Test
  def sizeCompareInt(): Unit = {
    val seq = Seq(1, 2, 3)
    assert(seq.sizeCompare(2) > 0)
    assert(seq.sizeCompare(3) == 0)
    assert(seq.sizeCompare(4) < 0)
  }

  @Test
  def sizeCompareIterable(): Unit = {
    def check[I1[X] <: Iterable[X], I2[X] <: Iterable[X]]
    (f1: IterableFactory[I1], f2: IterableFactory[I2]): Unit = {
      val it = f1(1, 2, 3)
      assert(it.sizeCompare(f2(1, 2)) > 0)
      assert(it.sizeCompare(f2(1, 2, 3)) == 0)
      assert(it.sizeCompare(f2(1, 2, 3, 4)) < 0)
    }

    // factories for `Seq`s with known and unknown size
    val known: IterableFactory[IndexedSeq] = Vector
    val unknown: IterableFactory[LinearSeq] = List

    check(known, known)
    check(known, unknown)
    check(unknown, known)
    check(unknown, unknown)
  }

  @Test def copyToArray(): Unit = {
    def check(a: Array[Int], copyToArray: Array[Int] => Int)(n: Int)(start: Int, end: Int) = {

      assertEquals(n, copyToArray(a))

      var i = 0
      while (i < start) {
        assertEquals(0, a(i))
        i += 1
      }
      while (i < a.length && i < end) {
        assertEquals(i - start, a(i))
        i += 1
      }
      while (i < a.length) {
        assertEquals(0, a(i))
        i += 1
      }
    }

    def checkUp(basis: IterableOnce[Int]): Unit = {
      val it = Iterable.from(basis)
      val far = 100000

      check(new Array(100), it.copyToArray(_))(100)(0, far)
      check(new Array(10), it.copyToArray(_))(10)(0, far)
      check(new Array(100), it.copyToArray(_))(100)(0, 100)

      check(new Array(100), it.copyToArray(_, 5))(95)(5, 105)
      check(new Array(10), it.copyToArray(_, 5))(5)(5, 10)
      check(new Array(1000), it.copyToArray(_, 5))(100)(5, 105)

      check(new Array(100), it.copyToArray(_, 5, 50))(50)(5, 55)
      check(new Array(10), it.copyToArray(_, 5, 50))(5)(5, 10)
      check(new Array(1000), it.copyToArray(_, 5, 50))(50)(5, 55)

      // bad start index throws if n > 0
      assertThrows[ArrayIndexOutOfBoundsException](it.copyToArray(new Array(10), start = -1))
      assertThrows[ArrayIndexOutOfBoundsException](it.copyToArray(new Array(10), start = -1, n = 10))
      check(new Array(10), it.copyToArray(_, start = -1, n = 0))(0)(0, 0)
      check(new Array(10), it.copyToArray(_, start = 1, n = -1))(0)(0, 0)

      check(new Array(10), it.copyToArray(_, 10))(0)(0, 0)
      check(new Array(10), it.copyToArray(_, 10, 10))(0)(0, 0)
      check(new Array(10), it.copyToArray(_, 0, -1))(0)(0, 0)

      // bad start index is ignored if n < 0
      check(new Array(10), it.copyToArray(_, -1, -1))(0)(0, 0)
      check(new Array(10), it.copyToArray(_, Int.MinValue, -1))(0)(0, 0)

      // also if destination is empty
      check(new Array(0), it.copyToArray(_, 10, 10))(0)(0, 0)
      check(new Array(0), it.copyToArray(_, Int.MinValue, -1))(0)(0, 0)
    }
    checkUp(Range(0, 100))
    checkUp(List.range(0, 100))
    checkUp(Vector.range(0, 100))
  }

  @Test @nowarn("cat=deprecation")
  def emptyToTraversable(): Unit = {
    assert(Iterable.empty == Array.empty.toIterable)
    assert(Iterable.empty == Array.empty.toTraversable)
    assert(Iterable.empty == Option.empty.toTraversable)
    assert(Set.empty      == BitSet.empty.toTraversable)
    assert(Iterable.empty == IndexedSeq.empty.toTraversable)
    assert(Iterable.empty == Iterable.empty.toTraversable)
    assert(Iterable.empty == Iterator.empty.toTraversable)
    assert(Iterable.empty == LinearSeq.empty.toTraversable)
    assert(Iterable.empty == List.empty.toTraversable)
    assert(Iterable.empty == Nil.toTraversable)
    assert(Map.empty      == Map.empty.toTraversable)
    assert(Iterable.empty == Seq.empty.toTraversable)
    assert(Set.empty      == Set.empty.toTraversable)
    assert(Iterable.empty == Traversable.empty.toTraversable)
    assert(Map.empty      == concurrent.TrieMap.empty.toTraversable)
    assert(Set.empty      == immutable.BitSet.empty.toTraversable)
    assert(Map.empty      == immutable.HashMap.empty.toTraversable)
    assert(Set.empty      == immutable.HashSet.empty.toTraversable)
    assert(Iterable.empty == immutable.IndexedSeq.empty.toTraversable)
    assert(Map.empty      == immutable.IntMap.empty.toTraversable)
    assert(Iterable.empty == immutable.Iterable.empty.toTraversable)
    assert(Iterable.empty == immutable.LinearSeq.empty.toTraversable)
    assert(Iterable.empty == immutable.List.empty.toTraversable)
    assert(Iterable.empty == immutable.Nil.toTraversable)
    assert(Map.empty      == immutable.ListMap.empty.toTraversable)
    assert(Set.empty      == immutable.ListSet.empty.toTraversable)
    assert(Map.empty      == immutable.LongMap.empty.toTraversable)
    assert((0 to 0)       == ((0 to 0): immutable.Range.Inclusive).toTraversable)
    assert((0 until 1)    == ((0 until 1): immutable.Range).toTraversable)
    assert(Map.empty      == immutable.Map.empty.toTraversable)
    assert(Iterable.empty == immutable.Seq.empty.toTraversable)
    assert(Set.empty      == immutable.Set.empty.toTraversable)
    assert(Iterable.empty == immutable.Stream.empty.toTraversable)
    assert(Iterable.empty == ("": String).toTraversable)
    assert(Iterable.empty == immutable.Traversable.empty.toTraversable)
    assert(Iterable.empty == immutable.Vector.empty.toTraversable)
    assert(Iterable.empty == mutable.ArrayBuffer.empty.toTraversable)
    assert(Iterable.empty == mutable.ArraySeq.empty.toTraversable)
    assert(Iterable.empty == mutable.ArrayStack.empty.toTraversable)
    assert(Set.empty      == mutable.BitSet.empty.toTraversable)
    assert(Iterable.empty == mutable.Buffer.empty.toTraversable)
    assert(Map.empty      == mutable.HashMap.empty.toTraversable)
    assert(Set.empty      == mutable.HashSet.empty.toTraversable)
    assert(Iterable.empty == mutable.IndexedSeq.empty.toTraversable)
    assert(Iterable.empty == mutable.Iterable.empty.toTraversable)
    assert(Map.empty      == mutable.ListMap.empty.toTraversable)
    assert(Map.empty      == mutable.LongMap.empty.toTraversable)
    assert(Map.empty      == mutable.Map.empty.toTraversable)
    assert(Iterable.empty == mutable.Queue.empty.toTraversable)
    assert(Iterable.empty == mutable.Seq.empty.toTraversable)
    assert(Iterable.empty == mutable.Stack.empty.toTraversable)
    assert(Iterable.empty == mutable.Traversable.empty.toTraversable)
    assert(Map.empty      == mutable.LinkedHashMap.empty.toTraversable)
    assert(Set.empty      == mutable.LinkedHashSet.empty.toTraversable)
    assert(Iterable.empty == mutable.ListBuffer.empty.toTraversable)
    assert(Map.empty      == mutable.OpenHashMap.empty.toTraversable)
    assert(Set.empty      == mutable.Set.empty.toTraversable)
    assert(Iterable.empty == mutable.Stack.empty.toTraversable)
    assert(Iterable.empty == mutable.StringBuilder.newBuilder.toTraversable)
    assert(Iterable.empty == mutable.Traversable.empty.toTraversable)
    assert(Iterable.empty == mutable.UnrolledBuffer.empty.toTraversable)
    assert(Map.empty      == mutable.WeakHashMap.empty.toTraversable)
    assert(Iterable.empty == mutable.WrappedArray.empty.toTraversable)
  }

  @Test
  def test_SI10631(): Unit = {
    val baselist = List(1, 2)
    var checklist = List.empty[Int]
    val lst = baselist.view.map { x =>
      checklist = x :: checklist
      x
    }

    assertEquals(2, lst.last)
    assertEquals(baselist.reverse, checklist)
  }

  @Test
  def unzip(): Unit = {
    val zipped = Seq((1, 'a'), (2, 'b'), (3, 'c'))
    val (s1, s2) = zipped.unzip
    assertTrue(Seq(1, 2, 3).sameElements(s1))
    assertTrue(Seq('a', 'b', 'c').sameElements(s2))
  }

  @Test
  def unzip3(): Unit = {
    val zipped = Seq((1, 'a', true), (2, 'b', false), (3, 'c', true))
    val (s1, s2, s3) = zipped.unzip3
    assertTrue(Seq(1, 2, 3).sameElements(s1))
    assertTrue(Seq('a', 'b', 'c').sameElements(s2))
    assertTrue(Seq(true, false, true).sameElements(s3))
  }

  @Test
  def overrideClassName(): Unit = {
    class Foo[+A] extends Iterable[A] {
      def iterator = Iterator.empty[A]
      override def className = "Fu"
    }
    val foo = new Foo
    assertEquals("Fu()", foo.toString)
  }

  @deprecated("Overrides deprecated stringPrefix", since="2.13.0")
  @Test
  def overrideStringPrefix(): Unit = {
    class Foo[+A] extends Iterable[A] {
      def iterator = Iterator.empty[A]
      override def stringPrefix = "Bar"

    }
    val foo = new Foo
    assertEquals("Bar()", foo.toString)
  }

  @deprecated("Overrides deprecated stringPrefix", since="2.13.0")
  @Test
  def overrideClassNameAndStringPrefix(): Unit = {
    class Foo[+A] extends Iterable[A] {
      def iterator = Iterator.empty[A]
      override def className = "Fu"
      override def stringPrefix = "Bar"

    }
    val foo = new Foo
    assertEquals("Fu()", foo.toString)
  }

  @Test
  def partitionMap(): Unit = {
    val (left, right) = Seq[Any](1, "1", 2, "2", 3, "3", 4, "4", 5, "5").partitionMap {
      case i: Int => Left(i)
      case s: String => Right(s)
    }
    assertEquals(Seq(1, 2, 3, 4 ,5), left)
    assertEquals(Seq("1", "2", "3", "4" ,"5"), right)
  }

  @deprecated("Uses deprecated hasDefiniteSize, extends HashMap", since="2.13.0")
  @Test
  def hasDefiniteSize(): Unit = {
    import scala.{collection => c}
    import scala.collection.{mutable => m, immutable => i}
    assertTrue(Some(1).hasDefiniteSize)
    assertTrue(None.hasDefiniteSize)
    assertTrue(Option(1).hasDefiniteSize)
    assertTrue(Array(1).hasDefiniteSize)
    assertTrue("a".hasDefiniteSize)
    assertTrue(c.BitSet(1).hasDefiniteSize)
    assertFalse(scala.io.Source.fromString("a").buffered.hasDefiniteSize)
    assertTrue(c.IndexedSeq(1).hasDefiniteSize)
    assertTrue(c.IndexedSeq(1).view.hasDefiniteSize)
    assertTrue(c.Iterable(1).hasDefiniteSize)
    assertFalse(c.Iterator(1).hasDefiniteSize)
    assertTrue(c.LinearSeq(1).hasDefiniteSize)
    assertTrue(c.Map(1 -> 1).hasDefiniteSize)
    assertTrue(c.Map(1 -> 1).view.hasDefiniteSize)
    assertTrue(c.Seq(1).hasDefiniteSize)
    assertTrue(c.Seq(1).view.hasDefiniteSize)
    assertTrue(c.Set(1).hasDefiniteSize)
    assertTrue(c.SortedMap(1 -> 1).hasDefiniteSize)
    assertTrue(c.SortedSet(1).hasDefiniteSize)
    assertTrue(i.BitSet(1).hasDefiniteSize)
    assertTrue(i.HashMap(1 -> 1).hasDefiniteSize)
    assertTrue(i.HashSet(1).hasDefiniteSize)
    assertTrue(i.IndexedSeq(1).hasDefiniteSize)
    assertTrue(i.IntMap(1 -> 1).hasDefiniteSize)
    assertTrue(i.Iterable(1).hasDefiniteSize)
    assertTrue(i.LinearSeq(1).hasDefiniteSize)
    assertTrue(i.List(1).hasDefiniteSize)
    assertTrue(i.ListMap(1 -> 1).hasDefiniteSize)
    assertTrue(i.ListSet(1).hasDefiniteSize)
    assertTrue(i.LongMap(1L -> 1).hasDefiniteSize)
    assertTrue(i.Map(1 -> 1).hasDefiniteSize)
    assertTrue(i.Nil.hasDefiniteSize)
    assertTrue((1L to 1L).hasDefiniteSize)
    assertTrue(i.Queue(1).hasDefiniteSize)
    assertTrue((1 to 1).hasDefiniteSize)
    assertTrue(i.Seq(1).hasDefiniteSize)
    assertTrue(i.Set(1).hasDefiniteSize)
    assertTrue(i.SortedMap(1 -> 1).hasDefiniteSize)
    assertTrue(i.SortedSet(1).hasDefiniteSize)
    assertFalse(i.Stream(1).hasDefiniteSize)
    assertTrue(i.TreeMap(1 -> 1).hasDefiniteSize)
    assertTrue(i.TreeSet(1).hasDefiniteSize)
    assertTrue(i.Vector(1).hasDefiniteSize)
    assertFalse(i.Vector(1).iterator.hasDefiniteSize)
    assertTrue(m.AnyRefMap(Nil -> 1).hasDefiniteSize)
    assertTrue(m.ArrayBuffer(1).hasDefiniteSize)
    assertTrue(m.ArrayBuffer(1).view.hasDefiniteSize)
    assertTrue(m.BitSet(1).hasDefiniteSize)
    assertTrue(m.Buffer(1).hasDefiniteSize)
    assertTrue(m.HashMap(1 -> 1).hasDefiniteSize)
    assertTrue(m.HashSet(1).hasDefiniteSize)
    assertTrue(m.IndexedSeq(1).hasDefiniteSize)
    assertTrue(m.Iterable(1).hasDefiniteSize)
    assertTrue(m.LinkedHashMap(1 -> 1).hasDefiniteSize)
    assertTrue(m.LinkedHashSet(1).hasDefiniteSize)
    assertTrue(m.ListBuffer(1).hasDefiniteSize)
    assertTrue(m.ListMap(1 -> 1).hasDefiniteSize)
    assertTrue(m.LongMap(1L -> 1).hasDefiniteSize)
    assertTrue(m.Map(1 -> 1).hasDefiniteSize)
    assertTrue((new m.HashMap[Int, m.Set[Int]] with m.MultiMap[Int, Int]).hasDefiniteSize) // deprecated extension
    assertTrue(m.OpenHashMap(1 -> 1).hasDefiniteSize)
    assertTrue(m.PriorityQueue(1).hasDefiniteSize)
    assertTrue(m.Queue(1).hasDefiniteSize)
    assertTrue(m.Seq(1).hasDefiniteSize)
    assertTrue(m.Set(1).hasDefiniteSize)
    assertTrue(m.SortedMap(1 -> 1).hasDefiniteSize)
    assertTrue(m.SortedSet(1).hasDefiniteSize)
    assertTrue(m.Stack(1).hasDefiniteSize)
    assertTrue((new m.StringBuilder()).hasDefiniteSize)
    assertTrue(m.TreeMap(1 -> 1).hasDefiniteSize)
    assertTrue(m.TreeSet(1).hasDefiniteSize)
    assertTrue(m.UnrolledBuffer(1).hasDefiniteSize)
    assertTrue(m.WeakHashMap(1 -> 1).hasDefiniteSize)
    assertFalse(scala.io.Source.fromString("hello").hasDefiniteSize)
    assertTrue((List(1), List(2)).zipped.hasDefiniteSize)
    assertTrue((List(1), List(2), List(3)).zipped.hasDefiniteSize)
  }

  class SingleUseIterable[A] private (xs: A*) extends IterableOnce[A] with IterableOnceOps[A, SingleUseIterable, SingleUseIterable[A]] with UseIterableOps[A, SingleUseIterable, SingleUseIterable[A]] {
    private var iterated = false
    override def iterator = {
      assertFalse("Attempted to re-iterate!", iterated)
      iterated = true
      Iterator(xs: _*)
    }
  }
  object SingleUseIterable {
    def apply[A](xs: A*): SingleUseIterable[A] = new SingleUseIterable(xs: _*)
  }
  trait UseIterableOps[A, CC[_], C] {
    def collect[B](pf: PartialFunction[A,B]): CC[B] = ???
    def drop(n: Int): C = ???
    def dropWhile(p: A => Boolean): C = ???
    def filter(p: A => Boolean): C = ???
    def filterNot(pred: A => Boolean): C = ???
    def flatMap[B](f: A => scala.collection.IterableOnce[B]): CC[B] = ???
    def flatten[B](implicit asIterable: A => scala.collection.IterableOnce[B]): CC[B] = ???
    def map[B](f: A => B): CC[B] = ???
    def scanLeft[B](z: B)(op: (B, A) => B): CC[B] = ???
    def slice(from: Int, until: Int): C = ???
    def span(p: A => Boolean): (C, C) = ???
    def take(n: Int): C = ???
    def takeWhile(p: A => Boolean): C = ???
    def tapEach[U](f: A => U): C = ???
    def zipWithIndex: CC[(A, Int)] = ???
  }
  class ZeroUseIterable[A] private () extends IterableOnce[A] with IterableOnceOps[A, ZeroUseIterable, ZeroUseIterable[A]] with UseIterableOps[A, ZeroUseIterable, ZeroUseIterable[A]] {
    override def iterator = fail("Attempted to iterate!")
    override def knownSize = 0
  }
  object ZeroUseIterable {
    def apply[A](): ZeroUseIterable[A] = new ZeroUseIterable()
  }

  // testing unknown size where iterator isEmpty/nonEmpty and iterator is traversed only once;
  // testing knownSize == 0 and iterator is not queried.

  @Test def `IterableOnceOps.sum consumes one iterator`: Unit = assertEquals(10, SingleUseIterable(1, 2, 3, 4).sum)
  @Test def `IterableOnceOps.sum of empty iterator`: Unit = assertEquals(0, SingleUseIterable[Int]().sum)
  @Test def `IterableOnceOps.sum consumes no iterator`: Unit = assertEquals(0, ZeroUseIterable[Int]().sum)
  @Test def `IterableOnceOps.sum of one iterator`: Unit = assertEquals(42, SingleUseIterable[Int](42).sum)

  @Test def `IterableOnceOps.product consumes one iterator`: Unit = assertEquals(24, SingleUseIterable(1, 2, 3, 4).product)
  @Test def `IterableOnceOps.product of empty iterator`: Unit = assertEquals(1, SingleUseIterable[Int]().product)
  @Test def `IterableOnceOps.product consumes no iterator`: Unit = assertEquals(1, ZeroUseIterable[Int]().product)
  @Test def `IterableOnceOps.product of one iterator`: Unit = assertEquals(42, SingleUseIterable[Int](42).product)

  @Test def `IterableOnceOps.min consumes one iterator`: Unit = assertEquals(27, SingleUseIterable(42, 27, 37).min)
  @Test def `IterableOnceOps.min of empty iterator`: Unit =
    assertThrows[UnsupportedOperationException](SingleUseIterable[Int]().min, _.contains("min"))
  @Test def `IterableOnceOps.min consumes no iterator`: Unit =
    assertThrows[UnsupportedOperationException](ZeroUseIterable[Int]().min, _.contains("min"))

  @Test def `IterableOnceOps.minBy consumes one iterator`: Unit = assertEquals(27, SingleUseIterable(42, 27, 37).minBy(_ * 2))
  @Test def `IterableOnceOps.minBy of empty iterator`: Unit =
    assertThrows[UnsupportedOperationException](SingleUseIterable[Int]().minBy(_ * 2), _.contains("minBy"))
  @Test def `IterableOnceOps.minBy consumes no iterator`: Unit =
    assertThrows[UnsupportedOperationException](ZeroUseIterable[Int]().minBy(_ * 2), _.contains("minBy"))

  @Test def `IterableOnceOps.minOption consumes one iterator`: Unit = assertEquals(Some(27), SingleUseIterable(42, 27, 37).minOption)
  @Test def `IterableOnceOps.minOption of empty iterator`: Unit = assertEquals(None, SingleUseIterable[Int]().minOption)
  @Test def `IterableOnceOps.minOption consumes no iterator`: Unit = assertEquals(None, ZeroUseIterable[Int]().minOption)

  @Test def `IterableOnceOps.minByOption consumes one iterator`: Unit = assertEquals(Some(27), SingleUseIterable(42, 27, 37).minByOption(_ * 2))
  @Test def `IterableOnceOps.minByOption of empty iterator`: Unit = assertEquals(None, SingleUseIterable[Int]().minByOption(_ * 2))
  @Test def `IterableOnceOps.minByOption consumes no iterator`: Unit = assertEquals(None, ZeroUseIterable[Int]().minByOption(_ * 2))

  @Test def `IterableOnceOps.max consumes one iterator`: Unit = assertEquals(42, SingleUseIterable(42, 27, 37).max)
  @Test def `IterableOnceOps.max of empty iterator`: Unit =
    assertThrows[UnsupportedOperationException](SingleUseIterable[Int]().max, _.contains("max"))
  @Test def `IterableOnceOps.max consumes no iterator`: Unit =
    assertThrows[UnsupportedOperationException](ZeroUseIterable[Int]().max, _.contains("max"))

  @Test def `IterableOnceOps.maxBy consumes one iterator`: Unit = assertEquals(42, SingleUseIterable(42, 27, 37).maxBy(_ * 2))
  @Test def `IterableOnceOps.maxBy of empty iterator`: Unit =
    assertThrows[UnsupportedOperationException](SingleUseIterable[Int]().maxBy(_ * 2), _.contains("maxBy"))
  @Test def `IterableOnceOps.maxBy consumes no iterator`: Unit =
    assertThrows[UnsupportedOperationException](ZeroUseIterable[Int]().maxBy(_ * 2), _.contains("maxBy"))

  @Test def `IterableOnceOps.maxOption consumes one iterator`: Unit = assertEquals(Some(42), SingleUseIterable(42, 27, 37).maxOption)
  @Test def `IterableOnceOps.maxOption of empty iterator`: Unit = assertEquals(None, SingleUseIterable[Int]().maxOption)
  @Test def `IterableOnceOps.maxOption consumes no iterator`: Unit = assertEquals(None, ZeroUseIterable[Int]().maxOption)

  @Test def `IterableOnceOps.maxByOption consumes one iterator`: Unit = assertEquals(Some(42), SingleUseIterable(42, 27, 37).maxByOption(_ * 2))
  @Test def `IterableOnceOps.maxByOption of empty iterator`: Unit = assertEquals(None, SingleUseIterable[Int]().maxByOption(_ * 2))
  @Test def `IterableOnceOps.maxByOption consumes no iterator`: Unit = assertEquals(None, ZeroUseIterable[Int]().maxByOption(_ * 2))

  @Test def `IterableOnceOps.reduceLeft consumes one iterator`: Unit = assertEquals(106, SingleUseIterable(42, 27, 37).reduceLeft(_ + _))
  @Test def `IterableOnceOps.reduceLeft of empty iterator`: Unit =
    assertThrows[UnsupportedOperationException](SingleUseIterable[Int]().reduceLeft(_ + _), _.contains("reduceLeft"))
  @Test def `IterableOnceOps.reduceLeft consumes no iterator`: Unit =
    assertThrows[UnsupportedOperationException](ZeroUseIterable[Int]().reduceLeft(_ + _), _.contains("reduceLeft"))

  @Test def `IterableOnceOps.reduceLeftOption consumes one iterator`: Unit = assertEquals(Some(10), SingleUseIterable(1, 2, 3, 4).reduceLeftOption(_ + _))
  @Test def `IterableOnceOps.reduceLeftOption of empty iterator`: Unit = assertEquals(None, SingleUseIterable[Int]().reduceLeftOption(_ + _))
  @Test def `IterableOnceOps.reduceLeftOption consumes no iterator`: Unit = assertEquals(None, ZeroUseIterable[Int]().reduceLeftOption(_ + _))

  @Test def `IterableOnceOps.reduceRight consumes one iterator`: Unit = assertEquals(106, SingleUseIterable(42, 27, 37).reduceRight(_ + _))
  @Test def `IterableOnceOps.reduceRight of empty iterator`: Unit =
    assertThrows[UnsupportedOperationException](SingleUseIterable[Int]().reduceRight(_ + _), _.contains("reduceLeft"))
  @Test def `IterableOnceOps.reduceRight consumes no iterator`: Unit =
    assertThrows[UnsupportedOperationException](ZeroUseIterable[Int]().reduceRight(_ + _), _.contains("reduceRight"))

  @Test def `IterableOnceOps.reduceRightOption consumes one iterator`: Unit = assertEquals(Some(10), SingleUseIterable(1, 2, 3, 4).reduceRightOption(_ + _))
  @Test def `IterableOnceOps.reduceRightOption of empty iterator`: Unit = assertEquals(None, SingleUseIterable[Int]().reduceRightOption(_ + _))
  @Test def `IterableOnceOps.reduceRightOption consumes no iterator`: Unit = assertEquals(None, ZeroUseIterable[Int]().reduceRightOption(_ + _))

  @Test def `IterableOnceOps.isEmpty consumes no iterator`: Unit = assertTrue(ZeroUseIterable[Int]().isEmpty)

  @Test def `sum uses my reduce if knownSize > 0`: Unit = {
    val reductive = new AbstractIterable[Int] {
      val values = List(1, 2, 3, 4, 32)
      override def iterator = Iterator.empty
      override def reduce[B >: Int](op: (B, B) => B): B = values.reduce(op)             // sum, produce
      override def reduceLeft[B >: Int](op: (B, Int) => B): B = values.reduceLeft(op)   // min, max
      override def knownSize = values.size
    }
    assertEquals(42,  reductive.sum)
    assertEquals(768, reductive.product)
    assertEquals(1,   reductive.min)
    assertEquals(32,  reductive.max)
  }
}
