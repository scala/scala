package scala.collection

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.immutable.{ArraySeq, List, Range}
import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class IterableTest {

  def f(xs: Seq[Seq[Int]], ys: Seq[Int]): Unit = {
    assert(xs.flatten == ys)
    assert(ys.flatMap(y => Some(y)) == ys.map(y => Some(y)).flatten)
  }

  @Test
  def flattenTest: Unit = {
    f(List(ArraySeq(1, 2, 3)), List(1, 2, 3))
  }

  @Test
  def concatTest: Unit = {
    val seq = Seq.concat(Seq(1, 2, 3), Iterable(4, 5, 6))
    assert(seq == Seq(1, 2, 3, 4, 5, 6))

    val vector = Vector.concat(Seq(1, 2, 3), Iterable(4, 5, 6))
    assert(vector == Vector(1, 2, 3, 4, 5, 6))

    val set = Set.concat(Seq(1, 2, 3), Iterable(3, 4, 5))
    assert(set == Set(1, 2, 3, 4, 5))

    val iterator = Iterator.concat(Seq(1, 2, 3), Seq(4, 5, 6))
    assert(iterator.toSeq == Seq(1, 2, 3, 4, 5, 6))
    assert(iterator.isEmpty)
  }

  @Test
  def groupMap(): Unit = {
    case class User(name: String, age: Int)

    def namesByAge(users: Set[User]): Map[Int, Set[String]] =
      users.groupMap(_.age)(_.name)

    val users =
      Set(User("Alice", 12), User("Bob", 14), User("Charlie", 12))
    val expected = Map(12 -> Set("Alice", "Charlie"), 14 -> Set("Bob"))
    Assert.assertEquals(expected, namesByAge(users))
  }

  @Test
  def groupMapReduce(): Unit = {
    def occurrences[A](as: Seq[A]): Map[A, Int] =
      as.groupMapReduce(identity)(_ => 1)(_ + _)

    val xs = Seq('a', 'b', 'b', 'c', 'a', 'a', 'a', 'b')
    val expected = Map('a' -> 4, 'b' -> 3, 'c' -> 1)
    Assert.assertEquals(expected, occurrences(xs))
  }

  @Test def copyToArray(): Unit = {
    def check(a: Array[Int], start: Int, end: Int) = {
      var i = 0
      while (i < start) {
        assert(a(i) == 0)
        i += 1
      }
      while (i < a.length && i < end) {
        assert(a(i) == i - start)
        i += 1
      }
      while (i < a.length) {
        assert(a(i) == 0)
        i += 1
      }
    }

    val far = 100000
    val l = Iterable.from(Range(0, 100))
    check(l.copyToArray(new Array(100)),
      0, far)
    check(l.copyToArray(new Array(10)),
      0, far)
    check(l.copyToArray(new Array(1000)),
      0, 100)

    check(l.copyToArray(new Array(100), 5),
      5, 105)
    check(l.copyToArray(new Array(10), 5),
      5, 10)
    check(l.copyToArray(new Array(1000), 5),
      5, 105)

    check(l.copyToArray(new Array(100), 5, 50),
      5, 55)
    check(l.copyToArray(new Array(10), 5, 50),
      5, 10)
    check(l.copyToArray(new Array(1000), 5, 50),
      5, 55)

    assertThrows[ArrayIndexOutOfBoundsException](l.copyToArray(new Array(10), -1))
    assertThrows[ArrayIndexOutOfBoundsException](l.copyToArray(new Array(10), -1, 10))

    check(l.copyToArray(new Array(10), 10),
      0, 0)
    check(l.copyToArray(new Array(10), 10, 10),
      0, 0)
    check(l.copyToArray(new Array(10), 0, -1),
      0, 0)
  }

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

    Assert.assertEquals(2, lst.last)
    Assert.assertEquals(baselist.reverse, checklist)
  }

  def unzip(): Unit = {
    val zipped = Seq((1, 'a'), (2, 'b'), (3, 'c'))
    val (s1, s2) = zipped.unzip
    Assert.assertTrue(Seq(1, 2, 3).sameElements(s1))
    Assert.assertTrue(Seq('a', 'b', 'c').sameElements(s2))
  }

  @Test
  def unzip3(): Unit = {
    val zipped = Seq((1, 'a', true), (2, 'b', false), (3, 'c', true))
    val (s1, s2, s3) = zipped.unzip3
    Assert.assertTrue(Seq(1, 2, 3).sameElements(s1))
    Assert.assertTrue(Seq('a', 'b', 'c').sameElements(s2))
    Assert.assertTrue(Seq(true, false, true).sameElements(s3))
  }

  @Test
  def overrideClassName: Unit = {
    class Foo[+A] extends Iterable[A] {
      def iterator = Iterator.empty[A]
      override def className = "Fu"
    }
    val foo = new Foo
    Assert.assertEquals("Fu()", foo.toString)
  }

  @Test
  def overrideStringPrefix: Unit = {
    class Foo[+A] extends Iterable[A] {
      def iterator = Iterator.empty[A]
      override def stringPrefix = "Bar"

    }
    val foo = new Foo
    Assert.assertEquals("Bar()", foo.toString)
  }

  @Test
  def overrideClassNameAndStringPrefix: Unit = {
    class Foo[+A] extends Iterable[A] {
      def iterator = Iterator.empty[A]
      override def className = "Fu"
      override def stringPrefix = "Bar"

    }
    val foo = new Foo
    Assert.assertEquals("Fu()", foo.toString)
  }

  @Test
  def overrideNewBuilder: Unit = {
    class Foo[+A] extends Iterable[A] {
      def iterator = Iterator.empty[A]
      override protected[this] def newBuilder: mutable.Builder[A, Seq[A]] = ???
    }
  }
}
