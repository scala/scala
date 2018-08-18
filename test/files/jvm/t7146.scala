
import scala.language.{ reflectiveCalls }
import java.util.concurrent.{Executor, ForkJoinPool}
import scala.concurrent._
import scala.util.control.NoStackTrace

object Test {
  def main(args: Array[String]): Unit = {
    ExecutionContext.global match {
      case fjp: ForkJoinPool =>
        val u = fjp.getUncaughtExceptionHandler

        println("ExecutionContext.global is a ForkJoinPool")
        println(s"should have non-null UncaughtExceptionHandler == ${u ne null}")

        if (u.toString startsWith "scala.concurrent.impl.ExecutionContextImpl")
          println("ExecutionContext.global.getUncaughtExceptionHandler is a scala.concurrent.impl.ExecutionContextImpl.")
        else
          println(s"!! ExecutionContext.global.executor.getUncaughtExceptionHandler == $u")

        print("should just print out on uncaught: ")
        u.uncaughtException(Thread.currentThread, new Throwable { override def printStackTrace(): Unit = { println("true") } })
      case other =>
        println(s"!! ExecutionContext.global == $other")
    }
  }
}
