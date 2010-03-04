import java.util.concurrent.Executors
import scala.actors.{Actor, SchedulerAdapter}
import Actor._

trait AdaptedActor extends Actor {
  override def scheduler =
    Test.scheduler
}

object One extends AdaptedActor {
  def act() {
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
  }
}

object Two extends AdaptedActor {
  def act() {
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
  }
}

object Test {
  val executor =
    Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors())

  val scheduler =
    new SchedulerAdapter {
      def execute(block: => Unit) {
        executor.execute(new Runnable {
          def run() { block }
        })
      }
    }

  def main(args: Array[String]) {
    One.start()
  }
}
