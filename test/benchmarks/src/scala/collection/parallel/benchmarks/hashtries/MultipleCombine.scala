package scala.collection.parallel.benchmarks
package hashtries




import collection.immutable.{HashMap => HashTrie}
import collection.mutable.HashMap






class MultipleCombine(val size: Int, val parallelism: Int, val runWhat: String) extends Bench with IntInit {
  var combines = 10
  
  var thattries = new Array[HashTrie[Int, Int]](combines)
  def initTries = for (r <- 0 until combines) {
    var thattrie = new HashTrie[Int, Int]
    for (i <- ((r + 1) * size) until ((r + 2) * size)) thattrie += ((i, i))
    thattries(r) = thattrie
  }
  initTries
  
  val thatmaps = new Array[HashMap[Int, Int]](10)
  def initMaps = for (r <- 0 until combines) {
    var thatmap = new HashMap[Int, Int]
    for (i <- ((r + 1) * size) until ((r + 2) * size)) thatmap += ((i, i))
    thatmaps(r) = thatmap
  }
  initMaps
  
  override def repetitionsPerRun = 25
  def runpar = throw new UnsupportedOperationException
  def runseq = runhashtrie
  def runhashtrie = {
    initHashTrie
    var trie = hashtrie
    for (r <- 0 until combines) trie = trie merge thattries(r)
  }
  def runappendtrie = {
    initHashTrie
    var trie = hashtrie
    for (r <- 0 until combines) trie = trie ++ thattries(r)
  }
  def runhashmap = {
    initHashMap
    var map = hashmap
    for (r <- 0 until combines) map = map ++ thatmaps(r)
  }
  def rundestructive = {
    initHashTrie
    var trie = hashtrie
    for (r <- 0 until combines) trie = trie merge thattries(r)
  }
  def companion = MultipleCombine
  def comparisonMap = Map("hashtrie" -> runhashtrie _, "hashmap" -> runhashmap _, "appendtrie" -> runappendtrie _, "destruct" -> rundestructive _)
  override def reset = runWhat match {
    case "appendtrie" => initHashTrie
    case "destruct" => initHashTrie
    case _ => super.reset
  }
}


object MultipleCombine extends BenchCompanion {
  def collectionName = "HashTrie"
  def benchName = "multi-combine";
  def apply(sz: Int, p: Int, what: String) = new MultipleCombine(sz, p, what)
  override def defaultSize = 5000
}















