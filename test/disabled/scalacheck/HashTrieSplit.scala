




import collection._




// checks whether hash tries split their iterators correctly
// even after some elements have been traversed
object Test {
  def main(args: Array[String]) {
    doesSplitOk
  }
  
  def doesSplitOk = {
    val sz = 2000
    var ht = new parallel.immutable.ParHashMap[Int, Int]
    // println("creating trie")
    for (i <- 0 until sz) ht += ((i + sz, i))
    // println("created trie")
    for (n <- 0 until (sz - 1)) {
      // println("---------> n = " + n)
      val pit = ht.parallelIterator
      val pit2 = ht.parallelIterator
      var i = 0
      while (i < n) {
        pit.next
        pit2.next
        i += 1
      }
      // println("splitting")
      val pits = pit.split
      val fst = pits(0).toSetUp
      val snd = pits(1).toSetUp
      val orig = pit2.toSetUp
      if (orig.size != (fst.size + snd.size) || orig != (fst ++ snd)) {
        println("Original: " + orig)
        println("First: " + fst)
        println("Second: " + snd)
        assert(false)
      }
    }
  }
}
