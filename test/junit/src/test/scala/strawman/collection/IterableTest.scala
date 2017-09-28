package strawman.collection

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.{ImmutableArray, List}

@RunWith(classOf[JUnit4])
class IterableTest {

  def f(xs: Seq[Seq[Int]], ys: Seq[Int]): Unit = {
    assert(xs.flatten == ys)
    assert(ys.flatMap(y => Some(y)) == ys.map(y => Some(y)).flatten)
  }

  @Test
  def flattenTest: Unit = {
    f(List(ImmutableArray(1, 2, 3)), List(1, 2, 3))
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

}
