//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

object Test extends App { test
  def test: Any = Await.result({
    def one = async { 1 }

    def test  = async {
      Option(true) match {
        case null | None => false
        case Some(v)     => await(one); v
      }
    }
    test
  }, Duration.Inf)
}
