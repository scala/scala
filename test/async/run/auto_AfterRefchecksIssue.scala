import scala.tools.nsc.transform.async.user.{async, autoawait}

trait Factory[T] {
  def create: T
}

sealed trait TimePoint

class TimeLine[TP <: TimePoint](val tpInitial: Factory[TP]) {
  @autoawait
  @async private[TimeLine] val tp: TP = tpInitial.create
  @autoawait
  @async def timePoint: TP = tp
}

object Test extends App { test
  def test: Unit = ()
}
