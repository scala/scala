// replaced all occurrences of 'Vector' with 'IndexedSeq'
import scala.collection.immutable.IndexedSeq
import scala.collection.SeqView

object Test {
  val funWithCCE = List.range(1,11).view.patch(5, List(100,101), 2)

  val v = new SeqView[Int, IndexedSeq[Int]] {
    def underlying = IndexedSeq(1,2,3)
    def apply(idx: Int) = underlying(idx)
    def length = underlying.length
    def iterator = underlying.iterator
  }
  val w = IndexedSeq(1, 2, 3).view

  def main(args: Array[String]): Unit = {
    println(v)
    println(w)
    println(go)
  }
  def go = v zip v
}
