
import akka.actor.{Actor, PoisonPill}
import Actor._
import akka.routing.{Routing, CyclicIterator}
import Routing._

import java.util.concurrent.CountDownLatch

object Pi extends App {

  calculate/*#*/(nrOfWorkers = 4, nrOfElements = 10000, nrOfMessages = 10000)

  // ====================
  // ===== Messages =====
  // ====================
  sealed trait PiMessage
  case object Calculate extends PiMessage/*#*/
  case class Work(start: Int, nrOfElements: Int) extends PiMessage
  case class Result(value: Double) extends PiMessage

  // ==================
  // ===== Worker =====
  // ==================
  class Worker extends Actor/*#*/ {

    // define the work
    def calculatePiFor(start: Int, nrOfElements: Int): Double = {
      var acc = 0.0
      for (i <- start until (start + nrOfElements))
        acc += 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
      acc
    }

    def receive /*?*/ = {
      case Work(start, nrOfElements) =>
        self reply/*#*/ Result(calculatePiFor(start, nrOfElements)) // perform the work // TODO: this currently returns wrong position for the symbol
    }
  }

  // ==================
  // ===== Master =====
  // ==================
  class Master(
    nrOfWorkers: Int, nrOfMessages: Int, nrOfElements: Int, latch: CountDownLatch)
    extends Actor {

    var pi: Double = _
    var nrOfResults: Int = _
    var start: Long = _

    // create the workers
    val workers = Vector.fill(nrOfWorkers)(actorOf[Worker]./*!*/start())
    
    // wrap them with a load-balancing router
    val router = Routing./*!*/loadBalancerActor(CyclicIterator(workers))./*!*/start()

    // message handler
    def receive = {
      case Calculate =>
        // schedule work
        //for (start <- 0 until nrOfMessages) router ! Work(start, nrOfElements)
        for (i <- 0 until nrOfMessages) router ! Work(i * nrOfElements, nrOfElements)

        // send a PoisonPill to all workers telling them to shut down themselves
        router./*!*/!(Broadcast(PoisonPill))

        // send a PoisonPill to the router, telling him to shut himself down
        router ! PoisonPill

      case Result(value) =>
        // handle result from the worker
        pi += value
        nrOfResults/*#*/ += 1
        if (nrOfResults == nrOfMessages) self./*!*/stop()
    }

    override def preStart() {
      start = System.currentTimeMillis
    }

    override def postStop() {
      // tell the world that the calculation is complete
      println(
        "\n\tPi estimate: \t\t%s\n\tCalculation time: \t%s millis"
        .format(pi, (System.currentTimeMillis - start)))
      latch/*#*/.countDown()
    }
  }

  // ==================
  // ===== Run it =====
  // ==================
  def calculate(nrOfWorkers: Int, nrOfElements: Int, nrOfMessages: Int) {

    // this latch is only plumbing to know when the calculation is completed
    val latch = new CountDownLatch(1)

    // create the master
    val master = actorOf(
      new Master(nrOfWorkers, nrOfMessages, nrOfElements, latch)).start()

    // start the calculation
    master ! Calculate

    // wait for master to shut down
    latch.await()
  }
}
