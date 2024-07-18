//> using options -Xasync

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async.{async, await}
object TestUtil {
  import language.implicitConversions
  implicit def lift[T](t: T): Future[T] = Future.successful(t)
  def block[T](f: Future[T]): T = Await.result(f, Duration.Inf)
}
import TestUtil._

object Test extends App {

  block(  async {
    var i = 100000000
    while (i > 0) {
      if (false) {
        await(())
      }
      i -= 1
    }
  })
}
