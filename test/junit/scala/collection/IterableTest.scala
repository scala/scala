package scala.collection

import org.junit.{Assert, Test}, Assert.{assertEquals, assertTrue}

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
    def check(a: Array[Int], copyToArray: Array[Int] => Int, elemsWritten: Int, start: Int, end: Int) = {

      assertEquals(copyToArray(a), elemsWritten)

      var i = 0
      while (i < start) {
        assertEquals(a(i),0)
        i += 1
      }
      while (i < a.length && i < end) {
        assertEquals(a(i), i - start)
        i += 1
      }
      while (i < a.length) {
        assertEquals(a(i), 0)
        i += 1
      }
    }

    val far = 100000
    val l = Iterable.from(Range(0, 100))
    check(new Array(100), l.copyToArray(_), 100, 0, far)
    check(new Array(10), l.copyToArray(_), 10, 0, far)
    check(new Array(100), l.copyToArray(_), 100, 0, 100)

    check(new Array(100), l.copyToArray(_, 5), 95, 5, 105)
    check(new Array(10), l.copyToArray(_, 5), 5, 5, 10)
    check(new Array(1000), l.copyToArray(_, 5), 100, 5, 105)

    check(new Array(100), l.copyToArray(_, 5, 50), 50, 5, 55)
    check(new Array(10), l.copyToArray(_, 5, 50), 5, 5, 10)
    check(new Array(1000), l.copyToArray(_, 5, 50), 50, 5, 55)

    assertThrows[ArrayIndexOutOfBoundsException]( l.copyToArray(new Array(10), -1))
    assertThrows[ArrayIndexOutOfBoundsException]( l.copyToArray(new Array(10), -1, 10))
    assertEquals(l.copyToArray(new Array(10), 1, -1), 0)

    check(new Array(10), l.copyToArray(_, 10), 0, 0, 0)
    check(new Array(10), l.copyToArray(_, 10, 10), 0, 0, 0)
    check(new Array(10), l.copyToArray(_, 0, -1), 0, 0, 0)
  }

  @deprecated("Uses deprecated toTraversable", since="2.13.0")
  @Test
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
    assertEquals(left, Seq(1, 2, 3, 4 ,5))
    assertEquals(right, Seq("1", "2", "3", "4" ,"5"))
  }

  @deprecated("Uses deprecated hasDefiniteSize, extends HashMap", since="2.13.0")
  @Test
  def hasDefiniteSize(): Unit = {
    import scala.{collection => c}
    import scala.collection.{mutable => m, immutable => i}
    assertEquals(true, Some(1).hasDefiniteSize)
    assertEquals(true, None.hasDefiniteSize)
    assertEquals(true, Option(1).hasDefiniteSize)
    assertEquals(true, Array(1).hasDefiniteSize)
    assertEquals(true, "a".hasDefiniteSize)
    assertEquals(true, c.BitSet(1).hasDefiniteSize)
    assertEquals(false, scala.io.Source.fromString("a").buffered.hasDefiniteSize)
    assertEquals(true, c.IndexedSeq(1).hasDefiniteSize)
    assertEquals(true, c.IndexedSeq(1).view.hasDefiniteSize)
    assertEquals(true, c.Iterable(1).hasDefiniteSize)
    assertEquals(false, c.Iterator(1).hasDefiniteSize)
    assertEquals(true, c.LinearSeq(1).hasDefiniteSize)
    assertEquals(true, c.Map(1 -> 1).hasDefiniteSize)
    assertEquals(true, c.Map(1 -> 1).view.hasDefiniteSize)
    assertEquals(true, c.Seq(1).hasDefiniteSize)
    assertEquals(true, c.Seq(1).view.hasDefiniteSize)
    assertEquals(true, c.Set(1).hasDefiniteSize)
    assertEquals(true, c.SortedMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, c.SortedSet(1).hasDefiniteSize)
    assertEquals(true, i.BitSet(1).hasDefiniteSize)
    assertEquals(true, i.HashMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, i.HashSet(1).hasDefiniteSize)
    assertEquals(true, i.IndexedSeq(1).hasDefiniteSize)
    assertEquals(true, i.IntMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, i.Iterable(1).hasDefiniteSize)
    assertEquals(true, i.LinearSeq(1).hasDefiniteSize)
    assertEquals(true, i.List(1).hasDefiniteSize)
    assertEquals(true, i.ListMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, i.ListSet(1).hasDefiniteSize)
    assertEquals(true, i.LongMap(1L -> 1).hasDefiniteSize)
    assertEquals(true, i.Map(1 -> 1).hasDefiniteSize)
    assertEquals(true, i.Nil.hasDefiniteSize)
    assertEquals(true, (1L to 1L).hasDefiniteSize)
    assertEquals(true, i.Queue(1).hasDefiniteSize)
    assertEquals(true, (1 to 1).hasDefiniteSize)
    assertEquals(true, i.Seq(1).hasDefiniteSize)
    assertEquals(true, i.Set(1).hasDefiniteSize)
    assertEquals(true, i.SortedMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, i.SortedSet(1).hasDefiniteSize)
    assertEquals(false, i.Stream(1).hasDefiniteSize)
    assertEquals(true, i.TreeMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, i.TreeSet(1).hasDefiniteSize)
    assertEquals(true, i.Vector(1).hasDefiniteSize)
    assertEquals(false, i.Vector(1).iterator.hasDefiniteSize)
    assertEquals(true, m.AnyRefMap(Nil -> 1).hasDefiniteSize)
    assertEquals(true, m.ArrayBuffer(1).hasDefiniteSize)
    assertEquals(true, m.ArrayBuffer(1).view.hasDefiniteSize)
    assertEquals(true, m.BitSet(1).hasDefiniteSize)
    assertEquals(true, m.Buffer(1).hasDefiniteSize)
    assertEquals(true, m.HashMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, m.HashSet(1).hasDefiniteSize)
    assertEquals(true, m.IndexedSeq(1).hasDefiniteSize)
    assertEquals(true, m.Iterable(1).hasDefiniteSize)
    assertEquals(true, m.LinkedHashMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, m.LinkedHashSet(1).hasDefiniteSize)
    assertEquals(true, m.ListBuffer(1).hasDefiniteSize)
    assertEquals(true, m.ListMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, m.LongMap(1L -> 1).hasDefiniteSize)
    assertEquals(true, m.Map(1 -> 1).hasDefiniteSize)
    assertTrue((new m.HashMap[Int, m.Set[Int]] with m.MultiMap[Int, Int]).hasDefiniteSize) // deprecated extension
    assertEquals(true, m.OpenHashMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, m.PriorityQueue(1).hasDefiniteSize)
    assertEquals(true, m.Queue(1).hasDefiniteSize)
    assertEquals(true, m.Seq(1).hasDefiniteSize)
    assertEquals(true, m.Set(1).hasDefiniteSize)
    assertEquals(true, m.SortedMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, m.SortedSet(1).hasDefiniteSize)
    assertEquals(true, m.Stack(1).hasDefiniteSize)
    assertEquals(true, (new m.StringBuilder()).hasDefiniteSize)
    assertEquals(true, m.TreeMap(1 -> 1).hasDefiniteSize)
    assertEquals(true, m.TreeSet(1).hasDefiniteSize)
    assertEquals(true, m.UnrolledBuffer(1).hasDefiniteSize)
    assertEquals(true, m.WeakHashMap(1 -> 1).hasDefiniteSize)
    assertEquals(false, scala.io.Source.fromString("hello").hasDefiniteSize)
    assertEquals(true, (List(1), List(2)).zipped.hasDefiniteSize)
    assertEquals(true, (List(1), List(2), List(3)).zipped.hasDefiniteSize)
  }
}
