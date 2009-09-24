import scala.actors.{Actor, DaemonActor}

/* Test that a daemon Actor that hasn't finished does not prevent termination */
object Test {

  class MyDaemon extends DaemonActor {
    def act() {
      react {
        case 'hello =>
          println("MSG1")
          reply()
          react {
            case 'bye =>
              println("done")
          }
      }
    }
  }

  def main(args: Array[String]) {
    val daemon = new MyDaemon
    daemon.start()
    Actor.actor {
      daemon !? 'hello
      println("MSG2")
    }
  }
}
