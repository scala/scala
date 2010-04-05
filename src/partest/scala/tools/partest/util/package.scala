/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools
package partest

import java.util.{ Timer, TimerTask }
import java.io.StringWriter
import nsc.io._

/** Misc code still looking for a good home.
 */
package object util {
  def interruptMeIn[T](seconds: Int)(body: => T): Option[T] = {
    val thisThread  = currentThread
    val alarm       = new SimpleAlarm(seconds * 1000) set thisThread.interrupt()

    try     { Some(body) }
    catch   { case _: InterruptedException => None }
    finally { alarm.cancel() ; Thread.interrupted() }
  }

  def allPropertiesString() = javaHashtableToString(System.getProperties)

  private def javaHashtableToString(table: java.util.Hashtable[_,_]) = {
    import collection.JavaConversions._
    (table.toList map { case (k, v) => "%s -> %s\n".format(k, v) }).sorted mkString
  }

  def filesToSet(pre: String, fs: List[String]): Set[AbstractFile] =
    fs flatMap (x => Option(AbstractFile getFile (Path(pre) / x).path)) toSet

  /** Copies one Path to another Path, trying to be sensible when one or the
   *  other is a Directory.  Returns true if it believes it succeeded.
   */
  def copyPath(from: Path, to: Path): Boolean = {
    if (!to.parent.isDirectory)
      to.parent.createDirectory(force = true)

    def copyDir = {
      val sub = to / from.name createDirectory true
      from.toDirectory.list forall (x => copyPath(x, sub))
    }
    (from.isDirectory, to.isDirectory) match {
      case (true, true)   => copyDir
      case (true, false)  => false
      case (false, true)  => from.toFile copyTo (to / from.name)
      case (false, false) => from.toFile copyTo to
    }
  }

  /**
  * Compares two files using a Java implementation of the GNU diff
  * available at http://www.bmsi.com/java/#diff.
  *
  * @param  f1  the first file to be compared
  * @param  f2  the second file to be compared
  * @return the text difference between the compared files
  */
  def diffFiles(f1: File, f2: File): String = {
    val diffWriter = new StringWriter
    val args = Array(f1.toAbsolute.path, f2.toAbsolute.path)

    io.DiffPrint.doDiff(args, diffWriter)
    val result = diffWriter.toString
    if (result == "No differences") "" else result
  }
}

package util {
  /** Set any number of alarms up with tuples of the form:
   *    seconds to alarm -> Function0[Unit] to execute
   */
  class Alarmer(alarmTimes: (Int, () => Unit)*) {
    import java.util.concurrent._

    val exec = Executors.newSingleThreadScheduledExecutor()
    private def sched(secs: Int, f: () => Unit) =
      exec.schedule(new Runnable { def run() = f() }, secs, TimeUnit.SECONDS)

    alarmTimes foreach (sched _).tupled
    exec.shutdown()

    def cancelAll() = exec.shutdownNow()
  }

  class SimpleAlarm(timeout: Long) {
    private val alarm = new Timer
    /** Start a timer, running the given body if it goes off.
     */
    def set(body: => Unit) = returning(new TimerTask { def run() = body })(alarm.schedule(_, timeout))

    /** Cancel the timer.
     */
    def cancel() = alarm.cancel()
  }

  // Thread.setDefaultUncaughtExceptionHandler(new UncaughtException)
  // class UncaughtException extends Thread.UncaughtExceptionHandler {
  //   def uncaughtException(t: Thread, e: Throwable) {
  //     Console.println("Uncaught in %s: %s".format(t, e))
  //   }
  // }
  //
  // lazy val logger = File("/tmp/partest.log").bufferedWriter()
  // def flog(msg: String) = logger synchronized {
  //   logger write (msg + "\n")
  //   logger.flush()
  // }
}