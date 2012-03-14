




import collection.parallel.mutable.ParCtrie


case class Entry(num: Double) {
  var sqrt = num
}


object Nums extends testing.Benchmark {
  val length = sys.props("length").toInt
  val par = sys.props("par").toInt
  var entries: Seq[Entry] = null
  var results: ParCtrie[Double, Entry] = null
  
  collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(par)
  
  override def setUp() {
    entries = (1 until length) map { num => Entry(num.toDouble) }
    results = ParCtrie()
    for (e <- entries) results += ((e.num, e))
  }
  
  def run() = {
    while (results.nonEmpty) {
      for ((num, e) <- results) {
        val nsqrt = 0.5 * (e.sqrt + e.num / e.sqrt)
        if (math.abs(nsqrt - e.sqrt) < 0.01) {
          results.remove(num)
        } else e.sqrt = nsqrt
      }
    }
  }
}

