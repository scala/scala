import java.util.concurrent.Executor
import scala.concurrent._
import scala.util.control.NoStackTrace

object Test {
  def main(args: Array[String]) {
    println("should be scala.concurrent.impl.ExecutionContextImpl == " +
      ExecutionContext.global.toString.startsWith("scala.concurrent.impl.ExecutionContextImpl"))
    val i = ExecutionContext.global.asInstanceOf[{ def executor: Executor }]
    println("should be scala.concurrent.forkjoin.ForkJoinPool == " +
      i.executor.toString.startsWith("scala.concurrent.forkjoin.ForkJoinPool"))
    val u = i.executor.
             asInstanceOf[{ def getUncaughtExceptionHandler: Thread.UncaughtExceptionHandler }].
             getUncaughtExceptionHandler
    println("should have non-null UncaughtExceptionHandler == " + (u ne null))
    println("should be a scala.concurrent.impl.ExecutionContextImpl UncaughtExceptionHandler == " +
      u.toString.startsWith("scala.concurrent.impl.ExecutionContextImpl"))
    print("should just print out on uncaught == ")
    u.uncaughtException(Thread.currentThread, new Throwable {
      override def printStackTrace() { println("true") }
    })
  }
}
