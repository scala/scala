
// default global execution context threads should terminate after interruption, when requested

import concurrent._
import util._
import java.util.concurrent.{CountDownLatch, TimeUnit}

object Test extends App with Runnable {
  implicit class LatchOps(c: CountDownLatch) {
    def waitForIt(): Boolean = c.await(1L, TimeUnit.SECONDS)
  }
  implicit lazy val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(null, _ => ())

  val started, goosed, done = new CountDownLatch(1)
  val group = new ThreadGroup("futuristic")
  val mainer = new Thread(group, this, "mainer")

  mainer.start()
  assert(started.waitForIt())
  group.interrupt()
  goosed.countDown()
  !done.waitForIt()
  show(0)

  def show(x: Int): Unit = {
    val ts = new Array[Thread](100)
    val n = group.enumerate(ts)
    //println(ts.take(n).map(_.getName).mkString("Group threads\n","\n",""))
    assert(n == x)
  }
  override def run(): Unit = {
    val f = Future {
      started.countDown()
      goosed.await()
      42
    }
    val g = f.transform {
      case x @ Success(v) => println(s"computed $v on ${Thread.currentThread}") ; x
      case Failure(e) => println(s"failed $e on ${Thread.currentThread}") ; Success(-1)
    }
    for (v <- g) {
      println(s"completed $v on ${Thread.currentThread}")
    }
  }
}
