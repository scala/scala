



import collection.parallel.mutable.ParCtrie



object Size extends testing.Benchmark {
  val length = sys.props("length").toInt
  val par = sys.props("par").toInt
  var parctrie = ParCtrie((0 until length) zip (0 until length): _*)
  
  collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(par)
  
  def run = {
    parctrie.size
  }
  
  var iteration = 0
  
  override def tearDown() {
    iteration += 1
    if (iteration % 4 == 0) parctrie = ParCtrie((0 until length) zip (0 until length): _*)
  }
  
}







