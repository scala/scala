package strawman
package collection.test

import org.junit.Test
import strawman.collection.Seq
import strawman.collection.immutable.{ImmutableArray, List}

import scala.{Int, Some, Unit}
import scala.Predef.{$conforms, assert}

class FlattenTest {

  def f(xs: Seq[Seq[Int]], ys: Seq[Int]): Unit = {
    assert(xs.flatten == ys)
    assert(ys.flatMap(y => Some(y)) == ys.map(y => Some(y)).flatten)
  }

  @Test
  def flattenTest: Unit = {
    f(List(ImmutableArray(1, 2, 3)), List(1, 2, 3))
  }

}
