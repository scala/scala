import scala.collection.mutable.ArrayBuffer
import scala.actors.Actor._
import scala.actors._
import scala.util._
import java.util.concurrent.{ TimeUnit, CountDownLatch }
import scala.concurrent.util.Duration
import scala.actors.pattern._

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

    val respActor = MigrationSystem.actorOf(Props(() => actor {
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
    }, "akka.actor.default-stash-dispatcher"))

    toStop += respActor

    respActor ! "bang"

    implicit val timeout = Timeout(Duration(500, TimeUnit.MILLISECONDS))
    val msg = ("bang qmark", 0L)
    val res1 = respActor.?(msg)(Timeout(Duration.Inf))
    append(res1().toString)
    latch.countDown()

    val msg1 = ("bang qmark", 1L)
    val res2 = respActor.?(msg1)(Timeout(Duration(500, TimeUnit.MILLISECONDS)))
    append((res2() match {
      case x: AskTimeoutException => None
      case v => Some(v)
    }).toString)
    latch.countDown()

    // this one should time out
    val msg11 = ("bang qmark", 500L)
    val res21 = respActor.?(msg11)(Timeout(Duration(1, TimeUnit.MILLISECONDS)))
    append((res21() match {
      case x: AskTimeoutException => None
      case v => Some(v)
    }).toString)
    latch.countDown()

    val msg2 = ("bang qmark in future", 0L)
    val fut1 = respActor.?(msg2)(Duration.Inf)
    append(fut1().toString())
    latch.countDown()

    val handler: PartialFunction[Any, String] = {
      case x: String => x.toString
    }

    val msg3 = ("typed bang qmark in future", 0L)
    val fut2 = (respActor.?(msg3)(Duration.Inf))
    append(Futures.future { handler.apply(fut2()) }().toString)
    latch.countDown()

    // output
    latch.await(200, TimeUnit.MILLISECONDS)
    if (latch.getCount() > 0) {
      println("Error: Tasks have not finished!!!")
    }

    buff.sorted.foreach(println)
    toStop.foreach(_ ! PoisonPill)
  }
}
