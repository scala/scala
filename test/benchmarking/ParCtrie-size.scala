



import collection.parallel.mutable.ParTrieMap



object Size extends testing.Benchmark {
  val length = sys.props("length").toInt
  val par = sys.props("par").toInt
  var parctrie = ParTrieMap((0 until length) zip (0 until length): _*)
  
  //collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(par)
  val ts = new collection.parallel.ForkJoinTaskSupport(new concurrent.forkjoin.ForkJoinPool(par))
  parctrie.tasksupport = ts
  
  def run = {
    parctrie.size
  }
  
  var iteration = 0
  
  override def tearDown() {
    iteration += 1
    if (iteration % 4 == 0) parctrie = ParTrieMap((0 until length) zip (0 until length): _*)
    parctrie.tasksupport = ts
  }
  
}







