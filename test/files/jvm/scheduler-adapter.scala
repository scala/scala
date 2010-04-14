import scala.actors.{Actor, SchedulerAdapter}

trait AdaptedActor extends Actor {
  override def scheduler =
    Test.adapted
}

object One extends AdaptedActor {
  def act() {
    try {
    Two.start()
    Two ! 'MsgForTwo
    react {
      case 'MsgForOne =>
        println("One: received msg")
    }
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}

object Two extends AdaptedActor {
  def act() {
    try {
    react {
      case 'MsgForTwo =>
        println("Two: received msg")
        One ! 'MsgForOne
    }
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
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
