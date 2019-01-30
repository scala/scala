import scala.tools.nsc.transform.async.user.{async, autoawait}

class TestReturnExprIssue(str: String) {
  @autoawait
  @async def getTestValue = Some(42)
  @autoawait
  @async def doStuff: Int = {
    val opt: Option[Int] = getTestValue // here we have an async method invoke
    opt match {
      case Some(li) => li // use the result somehow
      case None     =>
    }
    42 // type mismatch;   found   : AnyVal   required: Int
  }
}

object Test extends App { test
  @async def test: Unit = new TestReturnExprIssue("").doStuff
}
