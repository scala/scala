import scala.actors.Actor
import scala.actors.scheduler.ExecutorScheduler
import java.util.concurrent.Executors

object One extends AdaptedActor {
  def act() {
    try {
    Two.start()
    var i = 0
    loopWhile (i < Test.NUM_MSG) {
      i += 1
      Two ! 'MsgForTwo
      react {
        case 'MsgForOne =>
          if (i % (Test.NUM_MSG/10) == 0)
            println("One: OK")
      }
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
    var i = 0
    loopWhile (i < Test.NUM_MSG) {
      i += 1
      react {
        case 'MsgForTwo =>
          if (i % (Test.NUM_MSG/10) == 0)
            println("Two: OK")
          One ! 'MsgForOne
      }
    }
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}

trait AdaptedActor extends Actor {
  override def scheduler =
    Test.scheduler
}

object Test {
  val NUM_MSG = 100000

  val executor =
    Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors())

  val scheduler = ExecutorScheduler(executor)

  def main(args: Array[String]) {
    One.start()
  }
}
