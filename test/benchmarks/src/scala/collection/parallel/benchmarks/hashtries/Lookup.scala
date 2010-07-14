package scala.collection.parallel.benchmarks
package hashtries




import collection.immutable.{HashMap => HashTrie}
import collection.mutable.HashMap






class Lookup(val size: Int, val parallelism: Int, val runWhat: String) extends Bench with IntInit {
  def runpar = throw new UnsupportedOperationException
  def runseq = throw new UnsupportedOperationException
  def runhashmap = {
    var i = 0
    while (i < size) {
      hashmap(i)
      i += 1
    }
  }
  def runhashtrie = {
    var i = 0
    while (i < size) {
      hashtrie(i)
      i += 1
    }
  }
  def companion = Iterate
  def comparisonMap = Map("hashmap" -> runhashmap _, "hashtrie" -> runhashtrie _)
}


object Lookup extends BenchCompanion {
  def collectionName = "HashTrie"
  def benchName = "lookup";
  def apply(sz: Int, p: Int, what: String) = new Lookup(sz, p, what)
  override def defaultSize = 25000
}















