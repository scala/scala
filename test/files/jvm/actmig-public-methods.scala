import scala.collection.mutable.ArrayBuffer
import scala.actors.Actor._
import scala.actors._
import scala.actors.MigrationSystem
import scala.util.continuations._
import java.util.concurrent.{ TimeUnit, CountDownLatch }

object Test {
  val NUMBER_OF_TESTS = 6

  // used for sorting non-deterministic output
  val buff = ArrayBuffer[String]()
  val latch = new CountDownLatch(NUMBER_OF_TESTS)
  val toStop = ArrayBuffer[Actor]()

  def append(v: String) = synchronized {
    buff += v
  }

  def main(args: Array[String]) = {

    val respActor = actor {
      loop {
        react {
          case (x: String, time: Long) =>
            Thread.sleep(time)
            reply(x + " after " + time)
          case str: String =>
            append(str)
            latch.countDown()
          case _ => exit()
        }
      }
    }

    toStop += respActor

    respActor ! ("bang")

    val res1 = respActor !? (("bang qmark", 0L))
    append(res1.toString)
    latch.countDown()

    val res2 = respActor !? (200, ("bang qmark", 1L))
    append(res2.toString)
    latch.countDown()

    val res21 = respActor !? (1, ("bang qmark", 200L))
    append(res21.toString)
    latch.countDown()

    val fut1 = respActor !! (("bang qmark in future", 0L))
    append(fut1().toString())
    latch.countDown()

    val fut2 = respActor !! (("typed bang qmark in future", 0L), { case x: String => x })
    append(fut2())
    latch.countDown()

    // output
    latch.await(10, TimeUnit.MILLISECONDS)
    if (latch.getCount() > 0) {
      println("Error: Tasks have not finished!!!")
    }

    buff.sorted.foreach(println)
    toStop.foreach(_ ! 'stop)
  }
}
