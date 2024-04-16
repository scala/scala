//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

object Test extends App { test
  def func1() = async { "hello" }

  def func(a: Option[Boolean]) = async {a match {
    case null | None => await(func1()) + " world"
    case _           => "okay"
  }}
  def test: Any = Await.result(func(None), Duration.Inf)
}
