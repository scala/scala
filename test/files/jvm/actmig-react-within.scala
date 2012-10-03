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
  val finished = Promise[Boolean]

  def testReactWithin() = {
    val sActor = actor {
      loop {
        reactWithin(1) {
          case scala.actors.TIMEOUT =>
            println("received")
            exit()
          case _ =>
            println("Should not occur.")
        }
      }
    }

    val myActor = ActorDSL.actor(new StashingActor {
      context.setReceiveTimeout(1 millisecond)
      def receive = {
        case ReceiveTimeout =>
          println("received")
          finished.success(true)
          context.stop(self)
        case _ =>
          println("Should not occur.")
      }
    })
  }

  def main(args: Array[String]) = {
    testReactWithin()
    Await.ready(finished.future, 5 seconds)
  }

}
