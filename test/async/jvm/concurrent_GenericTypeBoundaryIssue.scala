//> using options -Xasync

import Test.test

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

trait InstrumentOfValue

trait Security[T <: InstrumentOfValue] extends InstrumentOfValue

class Bound extends Security[Bound]

class Futures extends Security[Futures]

object Test extends App { test
  val out = Console.out
  def processBound(bound: Bound): Future[Unit]  = async {
    out.println("process Bound")
  }
  def processFutures(futures: Futures): Future[Unit]  = async {
    out.println("process Futures")
  }
  def doStuff(sec: Security[_]): Future[Unit]  = async {
    sec match {
      case bound: Bound     => processBound(bound)
      case futures: Futures => processFutures(futures)
      case _                => throw new Exception("Unknown Security type: " + sec)
    }
  }

  def test: Unit = Await.result(doStuff(new Bound), Duration.Inf)
}
