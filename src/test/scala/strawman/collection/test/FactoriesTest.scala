package strawman
package collection

import immutable.List

import org.junit.Test
import org.junit.Assert._

import scala.Unit

class FactoriesTest {

  def apply(factory: IterableFactory[Iterable]): Unit = {
    assertTrue(factory(1, 2, 3).iterator().sameElements(View.Elems(1, 2, 3)))
  }

  def iterate(factory: IterableFactory[Iterable]): Unit = {
    val iterable = factory.iterate(0, 10)(x => x + 1)
    val expectedValues = immutable.Range(0, 10)
    assertEquals(expectedValues.size, iterable.size)
    assertTrue(iterable.forall(expectedValues.contains))
  }

  def range(factory: IterableFactory[Iterable]): Unit = {
    val iterable = factory.range(0, 10)
    val expectedValues = immutable.Range(0, 10)
    assertEquals(expectedValues.size, iterable.size)
    assertTrue(iterable.forall(expectedValues.contains))
  }

  def fill(factory: SeqFactory[Seq]): Unit = {
    val seq = factory.fill(10)(42)
    assertEquals(10, seq.size)
    assertTrue(seq.forall(_ == 42))

    val seq2 = factory.fill(10, 20)(42)
    assertEquals(10, seq2.size)
    seq2.foreach { seq3 =>
      assertEquals(20, seq3.size)
      assertTrue(seq3.forall(_ == 42))
    }
  }

  def tabulate(factory: SeqFactory[Seq]): Unit = {
    val seq = factory.tabulate(10)(x => x * x)
    assertEquals(10, seq.size)
    assertTrue(seq.zipWithIndex.forall { case (x, i) => x == i * i })

    val seq2 = factory.tabulate(10, 20)((i, j) => i * j)
    assertEquals(10, seq2.size)
    seq2.zipWithIndex.foreach { case (seq3, i) =>
      assertEquals(20, seq3.size)
      assertTrue(seq3.zipWithIndex.forall { case (x, j) => x == i * j })
    }
  }

  @Test
  def testFactories(): Unit = {
    val seqFactories: List[SeqFactory[Seq]] =
      List[SeqFactory[Seq]](
        immutable.List,
        immutable.LazyList,
        immutable.Vector,
        immutable.ImmutableArray,
        mutable.ListBuffer: SeqFactory[mutable.ListBuffer], // type ascription needed by dotty
        mutable.ArrayBuffer: SeqFactory[mutable.ArrayBuffer] // type ascription needed by dotty
      )

    val iterableFactories: List[IterableFactory[Iterable]] =
      (immutable.HashSet: IterableFactory[immutable.HashSet]) :: // type ascription needed by dotty
      mutable.HashSet ::
      (seqFactories: List[IterableFactory[Iterable]])

    iterableFactories.foreach(apply)
    iterableFactories.foreach(iterate)
    iterableFactories.foreach(range)

    seqFactories.foreach(fill)
    seqFactories.foreach(tabulate)
  }

}
