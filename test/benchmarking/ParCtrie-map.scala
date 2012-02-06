


import collection.parallel.mutable.ParCtrie



object Map extends testing.Benchmark {
  val length = sys.props("length").toInt
  val par = sys.props("par").toInt
  val parctrie = ParCtrie((0 until length) zip (0 until length): _*)
  
  collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(par)
  
  def run = {
    parctrie map {
      kv => kv
    }
  }
}

