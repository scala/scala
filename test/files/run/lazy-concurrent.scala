object Test {
  def main(args: Array[String]) {
    class Singleton {
      val field = ()
      println("Initializing singleton.")
    }
    lazy val Singleton = new Singleton

    var i = 0
    val threads = collection.mutable.ListBuffer[Thread]()
    while (i < 4) {
      val t = new Thread(new Runnable {
        def run = Singleton.field
      })
      threads += t
      t.start
      i += 1
    }
    threads.foreach(_.join)
  }
}
