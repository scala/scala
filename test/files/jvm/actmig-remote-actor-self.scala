/**
 * NOTE: Code snippets from this test are included in the Actor Migration Guide. In case you change
 * code in these tests prior to the 2.10.0 release please send the notification to @vjovanov.
 */
import scala.actors._
import scala.actors.migration._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.concurrent._
import scala.concurrent.duration._

object Test {
  val finished = Promise[Boolean]

  def main(args: Array[String]): Unit = {

    // can fail with class cast exception in alive
    val myAkkaActor = ActorDSL.actor(new StashingActor {
      override def preStart() = {
        alive(42013)
        println("registered")
        finished success true
        context.stop(self)
      }

      def receive = {
        case x: Int =>
      }
    })

    Await.result(finished.future, Duration.Inf).toString
  }

}
