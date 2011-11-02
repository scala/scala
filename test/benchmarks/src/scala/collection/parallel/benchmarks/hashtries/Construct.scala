package scala.collection.parallel.benchmarks
package hashtries




import collection.immutable.{HashMap => HashTrie}
import collection.mutable.HashMap






class Construct(val size: Int, val parallelism: Int, val runWhat: String) extends Bench {
  def reset {}
  
  def runpar = throw new UnsupportedOperationException
  def runseq = throw new UnsupportedOperationException
  def runhashmap = {
    val hashmap = new HashMap[Int, Int]
    for (i <- 0 until size) hashmap += ((i, i))
  }
  def runhashtrie = {
    var hashtrie = new HashTrie[Int, Int]
    for (i <- 0 until size) hashtrie += ((i, i))
  }
  
  def companion = Construct
  def comparisonMap = Map("hashmap" -> runhashmap _, "hashtrie" -> runhashtrie _)
}


object Construct extends BenchCompanion {
  def collectionName = "HashTrie"
  def benchName = "construct";
  def apply(sz: Int, p: Int, what: String) = new Construct(sz, p, what)
  override def defaultSize = 5000
}















