object Test {
  def main(args: Array[String]) {
    class Singleton {
      val field = ()
      println("Initializing singleton.")
    }
    lazy val Singleton = new Singleton

    var i = 0
    while (i < 4) {
      new Thread(new Runnable {
        def run = Singleton.field
      }).start
      i += 1
    }
  }
}
