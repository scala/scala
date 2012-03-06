


import collection.parallel.mutable.ParCtrie



object Map extends testing.Benchmark {
  val length = sys.props("length").toInt
  val par = sys.props("par").toInt
  val parctrie = ParCtrie((0 until length) zip (0 until length): _*)
  
  parctrie.tasksupport = new collection.parallel.ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(par))
  
  def run = {
    parctrie map {
      kv => kv
    }
  }
}

