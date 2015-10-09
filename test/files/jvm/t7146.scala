
import scala.language.{ reflectiveCalls }
import java.util.concurrent.Executor
import scala.concurrent._
import scala.util.control.NoStackTrace

object Test {
  def main(args: Array[String]): Unit = {
    val ec = ExecutionContext.global.toString
    if (ec startsWith "scala.concurrent.impl.ExecutionContextImpl")
      println("ExecutionContext.global is a scala.concurrent.impl.ExecutionContextImpl.")
    else println(s"!! ExecutionContext.global == $ec")

    val u = ExecutionContext.global.asInstanceOf[{ def executor: Executor }].executor.
             asInstanceOf[{ def getUncaughtExceptionHandler: Thread.UncaughtExceptionHandler }].
             getUncaughtExceptionHandler
    println(s"should have non-null UncaughtExceptionHandler == ${u ne null}")
    if (u.toString startsWith "scala.concurrent.impl.ExecutionContextImpl")
      println("ExecutionContext.global.executor.getUncaughtExceptionHandler is a scala.concurrent.impl.ExecutionContextImpl.")
    else println(s"!! ExecutionContext.global.executor.getUncaughtExceptionHandler == $u")

    print("should just print out on uncaught: ")
    u.uncaughtException(Thread.currentThread, new Throwable { override def printStackTrace() { println("true") } })
  }
}
