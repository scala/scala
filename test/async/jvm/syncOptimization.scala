//> using options -Xasync

import scala.tools.testkit.async.Async._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits._

object Test extends App {

  def stackDepth = Thread.currentThread().getStackTrace.length

  val future = async {
    val thread1 = Thread.currentThread
    val stackDepth1 = stackDepth

    val f = await(Future.successful(1))
    val thread2 = Thread.currentThread
    val stackDepth2 = stackDepth
    assert(thread1 == thread2)
    assert(stackDepth1 == stackDepth2)
  }
  Await.result(future, 10.seconds)

}
