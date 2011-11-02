package scala.collection.parallel.benchmarks
package hashtries




import collection.immutable.{HashMap => HashTrie}
import collection.mutable.HashMap



trait IntInit extends Bench {
  var hashmap: HashMap[Int, Int] = null
  var hashtrie: HashTrie[Int, Int] = null
  
  reset
  def reset = runWhat match {
    case "hashmap" => initHashMap
    case "hashtrie" => initHashTrie
    case "seq" => initHashTrie
  }
  def initHashTrie = {
    hashtrie = new HashTrie
    for (i <- 0 until size) hashtrie += ((i, i))
  }
  def initHashMap = {
    hashmap = new HashMap
    for (i <- 0 until size) hashmap += ((i, i))
  }

}
