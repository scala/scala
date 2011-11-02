package scala.collection.parallel.benchmarks
package hashtries




import collection.immutable.{HashMap => HashTrie}
import collection.mutable.HashMap






class Combine(val size: Int, val parallelism: Int, val runWhat: String) extends Bench with IntInit {
  var thattrie = new HashTrie[Int, Int]
  for (i <- size until 2 * size) thattrie += ((i, i))
  val thatmap = new HashMap[Int, Int]
  for (i <- size until 2 * size) thatmap += ((i, i))
  
  def runpar = throw new UnsupportedOperationException
  def runseq = runhashtrie
  def runhashtrie = {
    hashtrie merge thattrie
    // println
    // println("both tries:         " + HashTrie.bothtries)
    // println("one trie, one item: " + HashTrie.onetrie)
    // println("both single:        " + HashTrie.bothsingle)
    // System exit 1
  }
  def rundestructive = {
    hashtrie merge thattrie
  }
  def runappendtrie = hashtrie ++ thattrie
  def runhashmap = hashmap ++ thatmap
  def companion = Combine
  def comparisonMap = Map("hashtrie" -> runhashtrie _, "hashmap" -> runhashmap _, "destruct" -> rundestructive _, "appendtrie" -> runappendtrie _)
  override def reset = runWhat match {
    case "appendtrie" => initHashTrie
    case "destruct" => initHashTrie
    case _ => super.reset
  }
}


object Combine extends BenchCompanion {
  def collectionName = "HashTrie"
  def benchName = "combine";
  def apply(sz: Int, p: Int, what: String) = new Combine(sz, p, what)
  override def defaultSize = 5000
}















