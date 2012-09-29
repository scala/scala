/**
 * NOTE: Code snippets from this test are included in the Actor Migration Guide. In case you change
 * code in these tests prior to the 2.10.0 release please send the notification to @vjovanov.
 */
import scala.collection.mutable.ArrayBuffer
import scala.actors.Actor._
import scala.actors._
import scala.actors.migration._
import scala.util._
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.{ TimeUnit, CountDownLatch }
import scala.concurrent.duration._
import scala.actors.migration.pattern._
import scala.concurrent.ExecutionContext.Implicits.global

object Test {
  val NUMBER_OF_TESTS = 6

  // used for sorting non-deterministic output
  val buff = ArrayBuffer[String]()
  val latch = new CountDownLatch(NUMBER_OF_TESTS)
  val toStop = ArrayBuffer[ActorRef]()

  def append(v: String) = synchronized {
    buff += v
  }

  def main(args: Array[String]) = {

    val respActor = ActorDSL.actor(actor {
      loop {
        react {
          case (x: String, time: Long) =>
            Thread.sleep(time)
            reply(x + " after " + time)
          case str: String =>
            append(str)
            latch.countDown()
          case x =>
            exit()
        }
      }
    })

    toStop += respActor

    respActor ! "bang"

    {
      val msg = ("bang qmark", 0L)
      val res = respActor.?(msg)(Timeout(Duration.Inf))
      append(Await.result(res, Duration.Inf).toString)
      latch.countDown()
    }

    {
      val msg = ("bang qmark", 1L)
      val res = respActor.?(msg)(Timeout(5 seconds))

      val promise = Promise[Option[Any]]()
      res.onComplete(v => promise.success(v.toOption))
      append(Await.result(promise.future, Duration.Inf).toString)

      latch.countDown()
    }

    {
      val msg = ("bang qmark", 5000L)
      val res = respActor.?(msg)(Timeout(1 millisecond))
      val promise = Promise[Option[Any]]()
      res.onComplete(v => promise.success(v.toOption))
      append(Await.result(promise.future, Duration.Inf).toString)
      latch.countDown()
    }

    {
      val msg = ("bang bang in the future", 0L)
      val fut = respActor.?(msg)(Timeout(Duration.Inf))
      append(Await.result(fut, Duration.Inf).toString)
      latch.countDown()
    }

    {
      val handler: PartialFunction[Any, String] = {
        case x: String => x
      }

      val msg = ("typed bang bang in the future", 0L)
      val fut = (respActor.?(msg)(Timeout(Duration.Inf)))
      append((Await.result(fut.map(handler), Duration.Inf)).toString)
      latch.countDown()
    }

    // output
    latch.await(10, TimeUnit.SECONDS)
    if (latch.getCount() > 0) {
      println("Error: Tasks have not finished!!!")
    }

    buff.sorted.foreach(println)
    toStop.foreach(_ ! PoisonPill)
  }
}
