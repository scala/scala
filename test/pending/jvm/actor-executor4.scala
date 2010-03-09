import scala.actors.{Actor, Exit}
import scala.actors.scheduler.ExecutorScheduler
import java.util.concurrent.Executors

object One extends AdaptedActor {
  def act() {
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
  }
}

object Two extends AdaptedActor {
  def act() {
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
  }
}

trait AdaptedActor extends Actor {
  override def scheduler =
    Test.scheduler
}

object Test {
  val NUM_MSG = 100000

  val scheduler =
    ExecutorScheduler(
      Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors()),
      false)

  def main(args: Array[String]) {
    (new AdaptedActor {
      def act() {
        trapExit = true
        link(One)
        One.start()

        receive {
          case Exit(from, reason) =>
            println("One exited")
            Test.scheduler.shutdown()
        }
      }
    }).start()
  }
}
