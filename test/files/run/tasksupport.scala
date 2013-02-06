object Test extends App {

  def custom() {
    val customTaskSupport = new collection.parallel.ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(2))
    val pc = Array(1, 2, 3).parWith(customTaskSupport)
    val mapped = pc.map(_ + 1)
    assert(pc.tasksupport eq customTaskSupport, "custom task support not resolved")
    assert(mapped.tasksupport eq customTaskSupport, "custom task support not resolved")
   }

  custom()

}