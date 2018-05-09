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
}
