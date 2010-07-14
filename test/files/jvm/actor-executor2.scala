import scala.actors.{Actor, SchedulerAdapter, Exit}
import Actor._
import java.util.concurrent.{Executors, RejectedExecutionException}

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

  val scheduler =
    new SchedulerAdapter {
      def execute(block: => Unit) {
        val task = new Runnable {
          def run() { block }
        }
        try {
          executor.execute(task)
        } catch {
          case ree: RejectedExecutionException =>
            task.run() // run task on current thread
        }
      }
    }

  def main(args: Array[String]) {
    try {
    self.trapExit = true
    link(One)
    One.start()

    receive {
      case Exit(from, reason) =>
        println("One exited")
        Test.executor.shutdown()
    }
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}
