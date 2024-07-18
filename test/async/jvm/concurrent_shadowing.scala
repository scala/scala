//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

object Test extends App { test

  trait Foo

  trait Bar extends Foo

  def boundary = async { "" }
  def test: Unit = Await.result(async {
    (new Bar {}: Any) match {
      case foo: Bar =>
        await { boundary }
        0 match {
          case _ => foo; ()
        }
        ()
      case x => throw new MatchError(x)
    }
    ()
  }, Duration.Inf)
}
