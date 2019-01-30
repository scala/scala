import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { test
  @async
  def test: Any = {
    @autoawait def one = 1

    @async def test = {
      Option(true) match {
        case null | None => false
        case Some(v)     => one; v
      }
    }
  }
}
