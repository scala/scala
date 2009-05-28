import scala.actors.{Actor, SchedulerAdapter}

trait AdaptedActor extends Actor {
  override def scheduler =
    Test.adapted
}

object One extends AdaptedActor {
  def act() {
    Two.start()
    Two ! 'MsgForTwo
    react {
      case 'MsgForOne =>
        println("One: received msg")
    }
  }
}

object Two extends AdaptedActor {
  def act() {
    react {
      case 'MsgForTwo =>
        println("Two: received msg")
        One ! 'MsgForOne
    }
  }
}

object Test {
  val adapted =
    new SchedulerAdapter {
      def execute(block: => Unit) {
        println("before")
        block
      }
    }

  def main(args: Array[String]) {
    One.start()
  }
}
