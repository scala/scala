package scala.collection

import org.junit.Test

class ValueClassSeqOpsImpl(private val array: Array[Int]) extends AnyVal with IterableOnce[Int] with SeqOps[Int, Seq, ValueClassSeqOpsImpl] {
  def apply(i: Int): Int = array(i)
  def length: Int = array.length
  def iterator: Iterator[Int] = array.iterator
  def toIterable: Iterable[Int] = array.toIndexedSeq
  protected def coll: ValueClassSeqOpsImpl = this
  protected def fromSpecific(coll: IterableOnce[Int]): ValueClassSeqOpsImpl =
    new ValueClassSeqOpsImpl(coll.iterator.toArray)
  def iterableFactory: IterableFactory[Seq] = Seq
  protected def newSpecificBuilder: mutable.Builder[Int, ValueClassSeqOpsImpl] =
    Array.newBuilder[Int].mapResult(new ValueClassSeqOpsImpl(_))
  override def toString = mkString("ValueClassSeqOpsImpl(", ", ", ")")
}

class ValueClassSeqOpsTest {
  @Test
  def testPermutation(): Unit = {
    val array = Array(1, 2, 3)
    val wrapper = new ValueClassSeqOpsImpl(array)
    val permutations = wrapper.permutations.toSeq.map(_.toSeq)
    assert(permutations == List(1, 2, 3).permutations.toSeq)
  }
}
