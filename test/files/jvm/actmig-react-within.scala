import scala.actors.migration.MigrationSystem._
import scala.actors.Actor._
import scala.actors._
import scala.actors.migration._
import java.util.concurrent.{ TimeUnit, CountDownLatch }
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.util.duration._
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

    val myActor = MigrationSystem.actorOf(Props(() => new StashingActor {
      context.setReceiveTimeout(1 millisecond)
      def receive = {
        case ReceiveTimeout =>
          println("received")
          finished.success(true)
          context.stop(self)
        case _ =>
          println("Should not occur.")
      }
    }, "default-stashing-dispatcher"))
  }

  def main(args: Array[String]) = {
    testReactWithin()
    Await.ready(finished.future, 5 seconds)
  }

}