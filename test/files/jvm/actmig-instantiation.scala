/**
 * NOTE: Code snippets from this test are included in the Actor Migration Guide. In case you change
 * code in these tests prior to the 2.10.0 release please send the notification to @vjovanov.
 */
import scala.actors.migration._
import scala.actors.Actor._
import scala.actors._
import java.util.concurrent.{ TimeUnit, CountDownLatch }
import scala.collection.mutable.ArrayBuffer

class TestStashingActor extends StashingActor {

  def receive = { case v: Int => Test.append(v); Test.latch.countDown() }

}

object Test {
  val NUMBER_OF_TESTS = 5

  // used for sorting non-deterministic output
  val buff = ArrayBuffer[Int](0)
  val latch = new CountDownLatch(NUMBER_OF_TESTS)
  val toStop = ArrayBuffer[ActorRef]()

  def append(v: Int) = synchronized {
    buff += v
  }

  def main(args: Array[String]) = {
    // plain scala actor
    val a1 = actor {
      react { case v: Int => Test.append(v); Test.latch.countDown() }
    }
    a1 ! 100

    // simple instantiation
    val a2 = ActorDSL.actor(new TestStashingActor)
    a2 ! 200
    toStop += a2

    // actor of with scala actor
    val a3 = ActorDSL.actor(actor {
      react { case v: Int => Test.append(v); Test.latch.countDown() }
    })
    a3 ! 300

    // using the manifest
    val a4 = ActorDSL.actor(new TestStashingActor)
    a4 ! 400
    toStop += a4

    // deterministic part of a test
    // creation without actor
    try {
      val a3 = new TestStashingActor
      a3 ! -1
    } catch {
      case e: Throwable => println("OK error: " + e)
    }

    // actor double creation
    try {
      val a3 = ActorDSL.actor({
        new TestStashingActor
        new TestStashingActor
      })
      a3 ! -1
    } catch {
      case e: Throwable => println("OK error: " + e)
    }

    // actor nesting
    try {
      val a5 = ActorDSL.actor({
        val a6 = ActorDSL.actor(new TestStashingActor)
        toStop += a6
        new TestStashingActor
      })

      a5 ! 500
      toStop += a5
    } catch {
      case e: Throwable => println("Should not throw an exception: " + e)
    }

    // output
    latch.await(5, TimeUnit.SECONDS)
    if (latch.getCount() > 0) {
      println("Error: Tasks have not finished!!!")
    }

    buff.sorted.foreach(println)
    toStop.foreach(_ ! PoisonPill)
  }
}
