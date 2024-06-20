object Test extends App {
  val createPredef = new Runnable {
    def run = {
      val _ = Predef;
    }
  }
  val createVector = new Runnable {
    def run = {
      val _ = scala.collection.immutable.Vector;
    }
  }
  val t1 = new Thread(createPredef)
  val t2 = new Thread(createVector)
  t1.start()
  t2.start()
  t1.join()
  t2.join()
}
