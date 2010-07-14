package scala.collection.parallel.benchmarks
package hashtries




import collection.immutable.{HashMap => HashTrie}
import collection.mutable.HashMap






class Foreach(val size: Int, val parallelism: Int, val runWhat: String) extends Bench with IntInit {
  def runpar = throw new UnsupportedOperationException
  def runseq = runhashtrie
  def runhashmap = hashmap.foreach(n => ())
  def runhashtrie = hashtrie.foreach(n => ())
  def companion = Foreach
  def comparisonMap = Map("hashmap" -> runhashmap _, "hashtrie" -> runhashtrie _)
}


object Foreach extends BenchCompanion {
  def collectionName = "HashTrie"
  def benchName = "foreach-light";
  def apply(sz: Int, p: Int, what: String) = new Foreach(sz, p, what)
  override def defaultSize = 25000
}















