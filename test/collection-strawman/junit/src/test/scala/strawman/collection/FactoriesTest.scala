package strawman.collection

import org.junit.Assert.{assertEquals, assertTrue}
import strawman.collection.mutable.ArrayBuffer
import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.List

@RunWith(classOf[JUnit4])
class FactoriesTest {

  val seq: Seq[Int] = ArrayBuffer(1, 2, 3)

  @Test def buildFromUsesSourceCollectionFactory(): Unit = {

    def cloneCollection[A, C](xs: Iterable[A])(implicit bf: BuildFrom[xs.type, A, C]): C =
      bf.fromSpecificIterable(xs)(xs)

    Assert.assertEquals("ArrayBuffer", cloneCollection(seq).className)
  }

  @Test def factoryIgnoresSourceCollectionFactory(): Unit = {

    def cloneElements[A, C](xs: Iterable[A])(cb: Factory[A, C]): C =
      cb.fromSpecific(xs)

    Assert.assertEquals("List", cloneElements(seq)(Seq).className)
  }

  def apply(factory: IterableFactory[Iterable]): Unit = {
    assertTrue(factory(1, 2, 3).iterator().sameElements(new View.Elems(1, 2, 3)))
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
        immutable.ImmutableArray.untagged,
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

  implicitly[Factory[Char, String]]
  implicitly[Factory[Char, Array[Char]]]
  implicitly[Factory[Int, BitSet]]
  implicitly[Factory[Int, mutable.BitSet]]
  implicitly[Factory[Int, immutable.BitSet]]

}
