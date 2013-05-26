
/* Test that a daemon Actor that hasn't finished does not prevent termination */

@deprecated("Suppress warnings", since="2.11")
object Test {

  import scala.actors.{Actor, DaemonActor}
  class MyDaemon extends DaemonActor {
    def act() {
      try {
      react {
        case 'hello =>
          println("MSG1")
          reply()
          react {
            case 'bye =>
              println("done")
          }
      }
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
  }

  def main(args: Array[String]) {
    val daemon = new MyDaemon
    daemon.start()
    Actor.actor {
      try {
      daemon !? 'hello
      println("MSG2")
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
  }
}
