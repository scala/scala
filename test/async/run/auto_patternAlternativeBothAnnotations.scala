import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { test
  @async
  def test: Any = {
    @autoawait def func1() = "hello"

    @async def func(a: Option[Boolean]) = a match {
      case null | None => func1 + " world"
      case _           => "okay"
    }

    def test: Any = func(None)
  }
}
