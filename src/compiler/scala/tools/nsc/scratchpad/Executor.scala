package scala.tools.nsc.scratchpad

import java.io.{PrintStream, OutputStreamWriter, Writer}
import scala.runtime.ScalaRunTime.replStringOf
import java.lang.reflect.InvocationTargetException

object Executor {

  println("exec started")

  private var currentWriter: CommentWriter = null

  def ultimateCause(ex: Throwable): Throwable = ex match {
    case ex: InvocationTargetException =>
      ultimateCause(ex.getCause)
    case ex: ExceptionInInitializerError =>
      ultimateCause(ex.getCause)
    case ex =>
      ex
  }

  /** Execute module with given name, redirecting all output to given
   *  source inserter. Catch all exceptions and print stacktrace of underlying causes.
   */
  def execute(name: String, si: SourceInserter) {
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
      val clazz = Class.forName(name+"$")
      clazz.getField("$MODULE").get(null)
    } catch {
      case ex: Throwable =>
        ultimateCause(ex) match {
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

  def $show(x: Any): String = replStringOf(x, scala.Int.MaxValue)
}

class StopException extends Exception
