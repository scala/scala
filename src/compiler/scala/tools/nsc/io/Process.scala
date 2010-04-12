/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.nsc
package io

import concurrent.ThreadRunner
import scala.annotation.tailrec
import scala.util.Properties.{ isWin, isMac, lineSeparator }
import scala.util.control.Exception.catching
import java.lang.{ Process => JProcess, ProcessBuilder => JProcessBuilder }
import java.io.{ IOException, InputStream, OutputStream, BufferedReader, InputStreamReader, PrintWriter, File => JFile }
import java.util.concurrent.LinkedBlockingQueue

/** The <code>Process</code> object contains convenience functions
 *  for running external processes.
 *
 *  An example usage:
 *  <pre>
 *    io.Process("ls", cwd = io.File("/")) foreach println
 *  </pre>
 *
 *  See http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4109888
 *  for a dated list of the many obstacles to a clean interface.
 *
 *  This is not finished!! Do not rely upon it yet.
 *
 *  TODO - remove requirement that process complete before we
 *  can get an iterator.
 *
 *  @author   Paul Phillips
 *  @since    2.8
 */

object Process
{
  def javaVmArguments: List[String] = {
    import collection.JavaConversions._

    java.lang.management.ManagementFactory.getRuntimeMXBean().getInputArguments().toList
  }
  lazy val runtime = Runtime.getRuntime()

  class Pipe[T](xs: Seq[T], stringify: T => String) {
    def |(cmd: String): Seq[String] = {
      val p = Process(cmd)
      xs foreach (x => p.stdin println stringify(x))
      p.stdin.close()
      p.stdout.toList
    }
  }

  object Pipe {
    /* After importing this implicit you can say for instance
     *   xs | "grep foo" | "grep bar"
     * and it will execute shells and pipe input/output.  You can
     * also implicitly or explicitly supply a function which translates
     * the opening sequence into Strings; if none is given toString is used.
     *
     * Also, once you use :sh in the repl, this is auto-imported.
     */
    implicit def seqToPipelinedProcess[T]
      (xs: Seq[T])
      (implicit stringify: T => String = (x: T) => x.toString): Pipe[T] =
    {
      new Pipe(xs, stringify)
    }
  }

  private[Process] class ProcessBuilder(val pb: JProcessBuilder) {
    def this(cmd: String*) = this(new JProcessBuilder(cmd: _*))
    def start() = new Process(() => pb.start())

    def withOnlyEnv(env: Map[String, String]): this.type = {
      pb.environment.clear()
      withEnv(env)
    }

    def withEnv(env: Map[String,String]): this.type = {
      if (env != null) {
        val jmap = pb.environment()
        for ((k, v) <- env) jmap.put(k, v)
      }
      this
    }

    def withCwd(cwd: Path): this.type = {
      if (cwd != null)
        pb directory cwd.jfile

      this
    }
    def withRedirectedErrorStream(merged: Boolean): this.type = {
      pb redirectErrorStream merged
      this
    }

    override def toString() = "ProcessBuilder(%s)" format pb.command()
  }

  // This can be fleshed out if more variations come up
  private val shell: String => Array[String] =
    if (isWin) Array("cmd.exe", "/C", _)
    else Array("sh", "-c", _)

  /** Executes the given command line in a shell.
   *
   *  @param    command   the command line
   *  @return             a Process object
   */
  def apply(
    command: String,
    env: Map[String, String] = null,
    cwd: Path = null,
    redirect: Boolean = false
  ): Process =
      exec(shell(command), env, cwd, redirect)

  /** Executes the given command line.
   *
   *  @param    command   the command line
   *  @return             a Process object
   */
  def exec(
    command: Seq[String],
    env: Map[String, String] = null,
    cwd: Path = null,
    redirect: Boolean = false
  ): Process =
      new ProcessBuilder(command: _*) withEnv env withCwd cwd withRedirectedErrorStream redirect start
}
import Process._

class Process(processCreator: () => JProcess) extends Iterable[String] {
  lazy val process = processCreator()

  def exitValue(): Option[Int] =
    catching(classOf[IllegalThreadStateException]) opt process.exitValue()

  def waitFor() = process.waitFor()
  def destroy() = process.destroy()
  def rerun()   = new Process(processCreator)

  def slurp()   = _out.slurp()
  def stdout    = iterator
  def iterator  = _out.iterator
  def stderr    = _err.iterator
  lazy val stdin = new PrintWriter(_in, true)

  class StreamedConsumer(in: InputStream) extends Thread with Iterable[String] {
    private val queue   = new LinkedBlockingQueue[String]
    private val reader  = new BufferedReader(new InputStreamReader(in))

    private def finish() {
      // make sure this thread is complete, and close the process's stdin
      join()
      _in.close()
    }

    def slurp(): String = {
      finish()
      queue.toArray map (_ + lineSeparator) mkString
    }

    def iterator = {
      finish()
      new Iterator[String] {
        val it = queue.iterator()
        def hasNext = it.hasNext
        def next = it.next
      }
    }
    @tailrec override final def run() {
      reader.readLine match {
        case null =>
          reader.close()
        case x    =>
          queue put x
          run()
      }
    }
  }

  private val _err = createConsumer(process.getErrorStream)
  private val _out = createConsumer(process.getInputStream)
  private val _in  = process.getOutputStream()

  private def createConsumer(in: InputStream) = {
    val t = new StreamedConsumer(in)
    t.start()
    t
  }

  override def toString() = "Process(%s)" format process.toString()
}

