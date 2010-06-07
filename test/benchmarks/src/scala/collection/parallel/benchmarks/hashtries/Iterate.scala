package scala.collection.parallel.benchmarks
package hashtries




import collection.immutable.{HashMap => HashTrie}
import collection.mutable.HashMap






class Iterate(val size: Int, val parallelism: Int, val runWhat: String) extends Bench with IntInit {
  def runpar = throw new UnsupportedOperationException
  def runseq = throw new UnsupportedOperationException
  def runhashmap = {
    val it = hashmap.iterator
    while (it.hasNext) it.next
  }
  def runhashtrie = {
    val it = hashtrie.iterator
    while (it.hasNext) it.next
  }
  def companion = Iterate
  def comparisonMap = Map("hashmap" -> runhashmap _, "hashtrie" -> runhashtrie _)
}


object Iterate extends BenchCompanion {
  def collectionName = "HashTrie"
  def benchName = "iterate-light";
  def apply(sz: Int, p: Int, what: String) = new Iterate(sz, p, what)
  override def defaultSize = 25000
}















