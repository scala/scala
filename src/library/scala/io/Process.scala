/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io

import annotation.experimental
import concurrent.ThreadRunner
import util.Properties.{ isWin, isMac }
import util.control.Exception.catching
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

@experimental
object Process
{
  lazy val javaVmArguments = java.lang.management.ManagementFactory.getRuntimeMXBean().getInputArguments()
  lazy val runtime = Runtime.getRuntime()

  @experimental
  private[Process] class ProcessBuilder(val pb: JProcessBuilder)
  {
    def this(cmd: String*) = this(new JProcessBuilder(cmd.toArray: _*))
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

    def withCwd(cwd: File): this.type = {
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
    cwd: File = null,
    redirect: Boolean = false
  ): Process =
      exec(shell(command), env, cwd)

  /** Executes the given command line.
   *
   *  @param    command   the command line
   *  @return             a Process object
   */
  def exec(
    command: Seq[String],
    env: Map[String, String] = null,
    cwd: File = null,
    redirect: Boolean = false
  ): Process =
      new ProcessBuilder(command: _*) withEnv env withCwd cwd start
}
import Process._

@experimental
class Process(processCreator: () => JProcess) extends Iterable[String]
{
  lazy val process = processCreator()

  def exitValue(): Option[Int] =
    catching(classOf[IllegalThreadStateException]) opt process.exitValue()

  def waitFor() = process.waitFor()
  def destroy() = process.destroy()
  def rerun() = new Process(processCreator)

  def stdout    = iterator
  def iterator  = _out.iterator
  def stderr    = _err.iterator
  lazy val stdin = new PrintWriter(_in, true)

  class StreamedConsumer(in: InputStream) extends Thread with Iterable[String] {
    private val queue = new LinkedBlockingQueue[String]
    private val reader = new BufferedReader(new InputStreamReader(in))

    def iterator = {
      join()  // make sure this thread is complete
      new Iterator[String] {
        val it = queue.iterator()
        def hasNext = it.hasNext
        def next = it.next
      }
    }
    override def run() {
      reader.readLine match {
        case null =>
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

