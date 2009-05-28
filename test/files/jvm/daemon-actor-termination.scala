import scala.actors.{Actor, DefaultExecutorScheduler}

/* Test that a daemon Actor that hasn't finished does not prevent termination */

trait DaemonActor extends Actor {
  override def scheduler =
    Test.daemonSched
}

object Test {
  val daemonSched =
    new DefaultExecutorScheduler(true)

  class MyDaemon extends DaemonActor {
    def act() {
      println("MSG1")
      Thread.sleep(5000)
      println("done")
    }
  }
  def main(args: Array[String]) {
    val daemon = new MyDaemon
    daemon.start()
    Thread.sleep(500) // give the daemon a chance to start
    println("MSG2")
  }
}
