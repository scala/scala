//> using options -Xasync

import scala.tools.testkit.async.Async._
import scala.concurrent.{ExecutionContext, Future}

object Test {
  def foo(implicit ec: ExecutionContext) = {
    await(Future(4))
  }
}
