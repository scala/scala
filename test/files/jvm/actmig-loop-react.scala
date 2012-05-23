import scala.actors.MigrationSystem._
import scala.actors.Actor._
import scala.actors.{ Actor, StashingActor, ActorRef, Props, MigrationSystem, PoisonPill }
import java.util.concurrent.{ TimeUnit, CountDownLatch }
import scala.collection.mutable.ArrayBuffer

object Test {

  def testLoopWithConditionReact() = {
    // Snippet showing composition of receives
    // Loop with Condition Snippet - before
    val myActor = actor {
      var c = true
      loopWhile(c) {
        react {
          case x: Int =>
            // do task
            println("do task")
            if (x == 42) c = false
        }
      }
    }

    myActor.start()
    myActor ! 1
    myActor ! 42

    // Loop with Condition Snippet - migrated
    val myAkkaActor = MigrationSystem.actorOf(Props(() => new StashingActor {

      def receive = {
        case x: Int =>
          // do task
          println("do task")
          if (x == 42) context.stop(self)
      }
    }, "default-stashing-dispatcher"))
    myAkkaActor ! 1
    myAkkaActor ! 42
  }

  def testNestedReact() = {
    // Snippet showing composition of receives
    // Loop with Condition Snippet - before
    val myActor = actor {
      var c = true
      loopWhile(c) {
        react {
          case x: Int =>
            // do task
            println("do task " + x)
            if (x == 42) c = false
            else
              react {
                case y: String =>
                  println("do string " + y)
              }
            println("after react")
        }
      }
    }
    myActor.start()

    myActor ! 1
    myActor ! "I am a String"
    myActor ! 42

    Thread.sleep(100)
    println(myActor.getState)

    // Loop with Condition Snippet - migrated
    val myAkkaActor = MigrationSystem.actorOf(Props(() => new StashingActor {

      def receive = {
        case x: Int =>
          // do task
          println("do task " + x)
          if (x == 42)
            context.stop(self)
          else
            context.become(({
              case y: String =>
                println("do string " + y)
            }: Receive).andThen(x => {
              unstashAll()
              context.unbecome()
            }).orElse { case x => stash() })
      }
    }, "default-stashing-dispatcher"))

    myAkkaActor ! 1
    myAkkaActor ! "I am a String"
    myAkkaActor ! 42

  }

  def exceptionHandling() = {
    // Stashing actor with act and exception handler
    val myActor = MigrationSystem.actorOf(Props(() => new StashingActor {

      def receive = { case _ => println("Dummy method.") }
      override def act() = {
        loop {
          react {
            case "fail" =>
              throw new Exception("failed")
            case "work" =>
              println("working")
            case "die" =>
              exit()
          }
        }
      }

      override def exceptionHandler = {
        case x: Exception => println("scala got exception")
      }

    }, "default-stashing-dispatcher"))

    myActor ! "work"
    myActor ! "fail"
    myActor ! "die"

    Thread.sleep(100)
    // Stashing actor in Akka style
    val myAkkaActor = MigrationSystem.actorOf(Props(() => new StashingActor {
      def receive = PFCatch({
        case "fail" =>
          throw new Exception("failed")
        case "work" =>
          println("working")
        case "die" =>
          context.stop(self)
      }, { case x: Exception => println("akka got exception") })
    }, "default-stashing-dispatcher"))

    myAkkaActor ! "work"
    myAkkaActor ! "fail"
    myAkkaActor ! "die"
  }

  def main(args: Array[String]) = {
    testLoopWithConditionReact()
    Thread.sleep(100)
    exceptionHandling()
    Thread.sleep(100)
    testNestedReact()
  }

}

// As per Jim Mcbeath blog (http://jim-mcbeath.blogspot.com/2008/07/actor-exceptions.html)
class PFCatch(f: PartialFunction[Any, Unit], handler: PartialFunction[Exception, Unit])
  extends PartialFunction[Any, Unit] {

  def apply(x: Any) = {
    try {
      f(x)
    } catch {
      case e: Exception if handler.isDefinedAt(e) => handler(e)
    }
  }

  def isDefinedAt(x: Any) = f.isDefinedAt(x)
}

object PFCatch {
  def apply(f: PartialFunction[Any, Unit], handler: PartialFunction[Exception, Unit]) = new PFCatch(f, handler)
}
