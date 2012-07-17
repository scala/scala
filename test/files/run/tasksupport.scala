





object Test extends App {
  
  def default() {
    import collection.parallel.Implicits.defaultTaskSupport
    val pc = Array(1, 2, 3).par
    val mapped = pc.map(_ + 1)
    assert(pc.tasksupport eq collection.parallel.Implicits.defaultTaskSupport, "default task support not resolved")
    assert(mapped.tasksupport eq collection.parallel.Implicits.defaultTaskSupport, "default task support not resolved")
  }
  
  def custom() {
    implicit val customTaskSupport = new collection.parallel.ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(2))
    val pc = Array(1, 2, 3).par
    val mapped = pc.map(_ + 1)
    assert(pc.tasksupport eq customTaskSupport, "custom task support not resolved")
    assert(mapped.tasksupport eq customTaskSupport, "custom task support not resolved")
   }
  
  default()
  custom()
  
}
