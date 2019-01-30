import scala.tools.nsc.transform.async.user.{async, autoawait}

trait InstrumentOfValue

trait Security[T <: InstrumentOfValue] extends InstrumentOfValue

class Bound extends Security[Bound]

class Futures extends Security[Futures]

object TestGenericTypeBoundIssue {
  @autoawait
  @async def processBound(bound: Bound): Unit = {
    println("process Bound")
  }
  @autoawait
  @async def processFutures(futures: Futures): Unit = {
    println("process Futures")
  }
  @autoawait
  @async def doStuff(sec: Security[_]): Unit = {
    sec match {
      case bound: Bound     => processBound(bound)
      case futures: Futures => processFutures(futures)
      case _                => throw new Exception("Unknown Security type: " + sec)
    }
  }
}

object Test extends App { test
  @async def test: Unit = TestGenericTypeBoundIssue.doStuff(new Bound)
}
