// replaced all occurrences of 'Vector' with 'IndexedSeq'
import scala.collection.immutable.IndexedSeq
import scala.collection.IndexedSeqView

object Test {
  val v = new IndexedSeqView[Int, IndexedSeq[Int]] {
    def underlying = IndexedSeq(1,2,3)
    def apply(idx: Int) = underlying(idx)
    def length = underlying.length
  }
  val w = IndexedSeq(1, 2, 3).view

  def main(args: Array[String]): Unit = {
    println(v)
    println(w)
    println(go)
  }
  def go = v zip v
}
