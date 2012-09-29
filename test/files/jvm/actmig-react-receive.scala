/**
 * NOTE: Code snippets from this test are included in the Actor Migration Guide. In case you change
 * code in these tests prior to the 2.10.0 release please send the notification to @vjovanov.
 */
import scala.actors.Actor._
import scala.actors._
import scala.actors.migration._
import java.util.concurrent.{ TimeUnit, CountDownLatch }
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{ Promise, Await }

object Test {
  val finishedRS, finishedRS1, finishedRSC, finishedRSC1 = Promise[Boolean]
  def testComposition() = {
    // Snippet showing composition of receives
    // React Snippet - before
    val myActor = actor {
      // do before
      println("do before")
      receive {
        case x: Int =>
          // do task
          println("do task")
      }
      println("do in between")
      receive {
        case x: String =>
          // do string now
          println("do string")
      }
      println("do after")
      finishedRSC1.success(true)
    }
    myActor.start()
    myActor ! 1
    myActor ! "1"
    Await.ready(finishedRSC1.future, 5 seconds)

    // React Snippet - migrated
    val myAkkaActor = ActorDSL.actor(new StashingActor {
      override def preStart() = {
        println("do before")
      }

      def receive = ({
        case x: Int =>
          // do task
          println("do task")
      }: Receive) andThen { v =>
        context.become {
          case x: String =>
            //do string
            println("do string")
            context.stop(self)
        }
        println("do in between")
      }

      override def postStop() = {
        println("do after")
        finishedRSC.success(true)
      }

    })
    myAkkaActor ! 1
    myAkkaActor ! "1"
    Await.ready(finishedRSC.future, 5 seconds)
  }

  def main(args: Array[String]) = {
    // React Snippet - before
    val myActor = actor {
      // do before
      println("do before")
      receive {
        case x: Int =>
          // do task
          println("do task")
      }
      println("do after")
      finishedRS1.success(true)
    }
    myActor.start()
    myActor ! 1

    Await.ready(finishedRS1.future, 5 seconds)

    // React Snippet - migrated
    val myAkkaActor = ActorDSL.actor(new StashingActor {
      override def preStart() = {
        println("do before")
      }

      def receive = {
        case x: Int =>
          // do task
          println("do task")
          context.stop(self)
      }

      override def postStop() = {
        println("do after")
        finishedRS.success(true)
      }

    })
    myAkkaActor ! 1

    Await.ready(finishedRS.future, 5 seconds)
    // Starting composition test
    testComposition()

  }
}
