import scala.actors.MigrationSystem._
import scala.actors.Actor._
import scala.actors.{ Actor, StashingActor, ActorRef, Props, MigrationSystem, PoisonPill }
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
    val a2 = MigrationSystem.actorOf(Props(() => new TestStashingActor, "akka.actor.default-stash-dispatcher"))
    a2 ! 200
    toStop += a2

    // actor of with scala actor
    val a3 = actorOf(Props(() => actor {
      react { case v: Int => Test.append(v); Test.latch.countDown() }
    }, "akka.actor.default-stash-dispatcher"))
    a3 ! 300

    // using the manifest
    val a4 = actorOf(Props(() => new TestStashingActor, "akka.actor.default-stash-dispatcher"))
    a4 ! 400
    toStop += a4

    // deterministic part of a test
    // creation without actorOf
    try {
      val a3 = new TestStashingActor
      a3 ! -1
    } catch {
      case e => println("OK error: " + e)
    }

    // actorOf double creation
    try {
      val a3 = actorOf(Props(() => {
        new TestStashingActor
        new TestStashingActor
      }, "akka.actor.default-stash-dispatcher"))
      a3 ! -1
    } catch {
      case e => println("OK error: " + e)
    }

    // actorOf nesting
    try {
      val a5 = actorOf(Props(() => {
        val a6 = actorOf(Props(() => new TestStashingActor, "akka.actor.default-stash-dispatcher"))
        toStop += a6
        new TestStashingActor
      }, "akka.actor.default-stash-dispatcher"))

      a5 ! 500
      toStop += a5
    } catch {
      case e => println("Should not throw an exception: " + e)
    }

    // output
    latch.await(200, TimeUnit.MILLISECONDS)
    if (latch.getCount() > 0) {
      println("Error: Tasks have not finished!!!")
    }

    buff.sorted.foreach(println)
    toStop.foreach(_ ! PoisonPill)
  }
}
