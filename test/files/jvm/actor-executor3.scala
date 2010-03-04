import scala.actors.Actor
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

  val executor =
    Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors())

  val scheduler = ExecutorScheduler(executor)

  def main(args: Array[String]) {
    One.start()
  }
}
