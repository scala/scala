import scala.actors.DaemonActor

/* Test that a DaemonActor that hasn't finished does not prevent termination */

object Test {
  class MyDaemon extends DaemonActor {
    def act() {
      println("I'm going to make you wait.")
      Thread.sleep(5000)
      println("Ok, I'm done.")
    }
  }
  def main(args: Array[String]) {
    val daemon = new MyDaemon
    daemon.start()
    Thread.sleep(500) // give the daemon a chance to start
    println("I'm tired of waiting for you.  Good bye.")
  }
}
