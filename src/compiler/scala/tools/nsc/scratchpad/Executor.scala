package scala.tools.nsc.scratchpad

import java.io.{PrintStream, OutputStreamWriter, Writer}

import scala.runtime.ScalaRunTime.stringOf
import java.lang.reflect.InvocationTargetException
import scala.reflect.runtime.ReflectionUtils._

object Executor {

  println("exec started")

  private var currentWriter: CommentWriter = null

  /** Execute module with given name, redirecting all output to given
   *  source inserter. Catch all exceptions and print stacktrace of underlying causes.
   */
  def execute(name: String, si: SourceInserter, classLoader: ClassLoader = getClass.getClassLoader) {
    val oldSysOut = System.out
    val oldSysErr = System.err
    val oldConsOut = Console.out
    val oldConsErr = Console.err
    val oldCwr = currentWriter
    currentWriter = new CommentWriter(si)
    val newOut = new PrintStream(new CommentOutputStream(currentWriter))
    System.setOut(newOut)
    System.setErr(newOut)
    Console.setOut(newOut)
    Console.setErr(newOut)
    try {
      singletonInstance(classLoader, name)
    } catch {
      case ex: Throwable =>
        unwrapThrowable(ex) match {
          case _: StopException => ;
          case cause => cause.printStackTrace()
        }
    } finally {
      currentWriter.close()
      System.setOut(oldSysOut)
      System.setErr(oldSysErr)
      Console.setOut(oldConsOut)
      Console.setErr(oldConsErr)
      currentWriter = oldCwr
    }
  }

  def $skip(n: Int) = currentWriter.skip(n)

  def $stop() = throw new StopException

  def $show(x: Any): String = stringOf(x, scala.Int.MaxValue)
}

class StopException extends Exception
