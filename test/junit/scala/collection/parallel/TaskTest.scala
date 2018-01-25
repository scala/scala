package scala.collection.parallel

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import java.util.concurrent.{ForkJoinPool, ForkJoinWorkerThread}, ForkJoinPool._

@RunWith(classOf[JUnit4])
class TaskTest {
  @Test
  def `t10577 task executes on foreign pool`(): Unit = {
    def mkFactory(name: String) = new ForkJoinWorkerThreadFactory {
      override def newThread(pool: ForkJoinPool) = {
        val t = new ForkJoinWorkerThread(pool) {}
        t.setName(name)
        t
      }
    }
    def mkPool(name: String) = new ForkJoinPool(1, mkFactory(name), null, false)

    val one = List(1).par
    val two = List(2).par

    one.tasksupport = new ForkJoinTaskSupport(mkPool("one"))
    two.tasksupport = new ForkJoinTaskSupport(mkPool("two"))

    for (x <- one ; y <- two) assert(Thread.currentThread.getName == "two")
  }
}
