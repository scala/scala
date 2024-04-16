//> using options -Xasync

import scala.concurrent._, ExecutionContext.Implicits.global, scala.tools.testkit.async.Async._

trait Factory[T] {
  def create: T
}

sealed trait TimePoint

class TimeLine[TP <: TimePoint](val tpInitial: Factory[TP]) {
  private[TimeLine] val tp: Future[TP] = async { tpInitial.create }
  def timePoint: Future[TP] = async { await(tp) }
}

object Test extends App { test
  def test: Unit = ()
}
