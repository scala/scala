


import collection.parallel.immutable.ParVector



object Reduce extends testing.Benchmark {
  val length = sys.props("length").toInt
  val par = sys.props("par").toInt
  val parvector = ParVector((0 until length): _*)
  
  parvector.tasksupport = new collection.parallel.ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(par))
  
  def run = {
    parvector reduce {
      (a, b) => a + b
    }
  }
}


object ReduceSeq extends testing.Benchmark {
  val length = sys.props("length").toInt
  val vector = collection.immutable.Vector((0 until length): _*)
  
  def run = {
    vector reduce {
      (a, b) => a + b
    }
  }
}

