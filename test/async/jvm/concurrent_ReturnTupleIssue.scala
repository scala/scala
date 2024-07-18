//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

class TestReturnExprIssue(str: String) {
  def getTestValue = async(Some(42))
  def doStuff: Future[Int]  = async {
    val opt: Option[Int] = await(getTestValue) // here we have an async method invoke
    opt match {
      case Some(li) => li // use the result somehow
      case None     =>
    }
    42 // type mismatch;   found   : AnyVal   required: Int
  }
}

object Test extends App { test
  def test: Unit = Await.result(new TestReturnExprIssue("").doStuff, Duration.Inf)
}
