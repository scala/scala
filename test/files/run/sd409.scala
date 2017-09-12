import reflect.runtime.universe._

object Test {
  def main(args: Array[String]): Unit = {
    val threads = collection.mutable.Buffer[Thread]()
    for (i <- 1 to 22; j <- 1 to 8) {
      val t = new Thread {
        override def run(): Unit = {
          internal.reificationSupport.SyntacticTuple.apply(List.fill(i)(EmptyTree))
        }
      }
      threads += t
      t.start()
    }
    threads.foreach(_.join())
  }
}
