package scala.collection

import org.junit.{Assert, Test}

class ArrayWrapper(private val array: Array[Int]) extends AnyVal with IterableOnce[Int] with SeqOps[Int, Seq, ArrayWrapper] {
  def apply(i: Int): Int = array(i)
  def length: Int = array.length
  def iterator: Iterator[Int] = array.iterator
  def toIterable: Iterable[Int] = array.toIndexedSeq
  protected def coll: ArrayWrapper = this
  protected def fromSpecific(coll: IterableOnce[Int]): ArrayWrapper =
    new ArrayWrapper(coll.iterator.toArray)
  def iterableFactory: IterableFactory[Seq] = Seq
  protected def newSpecificBuilder: mutable.Builder[Int, ArrayWrapper] =
    Array.newBuilder[Int].mapResult(new ArrayWrapper(_))

  override def toString = mkString("ArrayWrapper(", ", ", ")")
}

class ArrayWrapperTest {

  @Test
  def testPermutation(): Unit = {
    val array = Array(1, 2, 3)
    val wrapper = new ArrayWrapper(array)
    val permutations = wrapper.permutations.toSeq
    println(permutations)
  }

}
