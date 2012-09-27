/**
 * NOTE: Code snippets from this test are included in the Actor Migration Guide. In case you change
 * code in these tests prior to the 2.10.0 release please send the notification to @vjovanov.
 */
import scala.actors._

class ReactorActor extends Actor {
  def act() {
    var cond = true
    loopWhile(cond) {
      react {
        case x: String if x == "hello1" => println("hello")
        case "exit" => cond = false
      }
    }
  }
}

class ReplyActor extends Actor {
  def act() {
    var cond = true
    loopWhile(cond) {
      react {
        case "hello" => println("hello")
        case "exit" => cond = false;
      }
    }
  }
}

object Test {

  def main(args: Array[String]) {
    val reactorActor = new ReactorActor
    val replyActor = new ReplyActor
    reactorActor.start()
    replyActor.start()

    reactorActor ! "hello1"
    replyActor ! "hello"

    reactorActor ! "exit"
    replyActor ! "exit"
  }
}