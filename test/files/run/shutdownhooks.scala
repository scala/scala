//> using javaOpt -Dneeds.forked.jvm

object Test {
  scala.sys.addShutdownHook {
    // sleep is added here so main#shutdown happens before this hook.
    // Thread.sleep(1000) was not enough according to https://github.com/scala/bug/issues/11536
    Thread.sleep(3000)
    println("Test#shutdown.")
  }

  def daemon() = {
    val t = new Thread {
      override def run(): Unit = {
        Thread.sleep(10000)
        println("Hallelujah!") // should not see this
      }
    }
    t.setDaemon(true)
    t.start()
    t
  }

  def nonDaemon() = {
    val t = new Thread {
      override def run(): Unit = {
        Thread.sleep(100)
        println("Fooblitzky!")
      }
    }
    t.start()
    t
  }

  def main(args: Array[String]): Unit = {
    daemon()
    nonDaemon()
    scala.sys.addShutdownHook {
      println("main#shutdown.")
    }
  }
}
