import java.util.concurrent.Executors
import scala.actors.{Actor, SchedulerAdapter}
import Actor._

trait AdaptedActor extends Actor {
  override def scheduler =
    Test.scheduler
}

object One extends AdaptedActor {
  def act() {
    try {
    Two.start()
    var i = 0
    loopWhile (i < 10000) {
      i += 1
      Two ! 'MsgForTwo
      react {
        case 'MsgForOne =>
          if (i % 1000 == 0)
            println("One: OK")
          if (i == 10000)
            Test.executor.shutdown()
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
    loopWhile (i < 10000) {
      i += 1
      react {
        case 'MsgForTwo =>
          if (i % 1000 == 0)
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

object Test {
  val executor =
    Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors())

  val scheduler =
    new SchedulerAdapter {
      def execute(block: => Unit) {
        val task = new Runnable {
          def run() { block }
        }
        try {
          executor.execute(task)
        } catch {
          case ree: java.util.concurrent.RejectedExecutionException =>
            task.run()
        }
      }
    }

  def main(args: Array[String]) {
    One.start()
  }
}
