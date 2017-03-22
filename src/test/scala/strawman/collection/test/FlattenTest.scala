package strawman
package collection.test

import org.junit.Test
import strawman.collection.Iterable
import strawman.collection.immutable.{List, HashSet}

import scala.{Int, Some, Unit}
import scala.Predef.$conforms

class FlattenTest {

  def f(xs: Iterable[Iterable[Int]], ys: Iterable[Int]): Unit = {
    xs.flatten                   // Iterable[Iterable[Int]] => Iterable[Int]
    ys.flatMap(y => Some(y))     // Iterable[Option[Int]]   => Iterable[Int]
    ys.map(y => Some(y)).flatten // Iterable[Option[Int]]   => Iterable[Int]
  }

//  @Test
  def flattenTest: Unit = {

    f(List(HashSet(1, 2, 3)), List(1, 2, 3))

  }

}
