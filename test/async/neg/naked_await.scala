//> using options -Xasync

import scala.tools.testkit.async.Async._
import scala.concurrent.{ExecutionContext, Future}

object Test {
  def foo(implicit ec: ExecutionContext) = {
    async {
      await(Future(1)) + await(Future(2))
      def foo = await(Future(3))
    }
    await(Future(4))
  }
}
