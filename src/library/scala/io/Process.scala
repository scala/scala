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
import java.io.{ InputStream, OutputStream, BufferedReader, InputStreamReader, File => JFile }

/** The <code>Process</code> object contains convenience functions
 *  for running external processes.
 *
 *  An example usage:
 *  <pre>
 *    io.Process.shell("ls", cwd = io.File("/")) foreach println
 *  </pre>
 *
 *  See http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4109888
 *  for a dated list of the many obstacles to a clean interface.
 *
|*  This is not finished!! Do not rely upon it yet.
 *
 *  @author   Paul Phillips
 *  @since    2.8
 */

@experimental
object Process
{
  lazy val runtime = Runtime.getRuntime()

  @experimental
  private[Process] class ProcessBuilder(val pb: JProcessBuilder)
  {
    def this(cmd: String*) = this(new JProcessBuilder(cmd.toArray: _*))

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
        pb directory cwd.file

      this
    }

    def start(): Process = new Process(pb.start())
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
  def shell(
    command: String,
    env: Map[String, String] = null,
    cwd: File = null
  ): Process =
      apply(shell(command), env, cwd)

  /** Executes the given command line.
   *
   *  @param    command   the command line
   *  @return             a Process object
   */
  def apply(
    command: Seq[String],
    env: Map[String, String] = null,
    cwd: File = null
  ): Process =
      new ProcessBuilder(command: _*) withEnv env withCwd cwd start
}
import Process._

@experimental
class Process(val process: JProcess) extends Iterable[String]
{
  class StreamedConsumer(in: InputStream) extends Thread with Iterable[String] {
    private val reader = new BufferedReader(new InputStreamReader(in))
    private lazy val stream: Stream[String] =
      Stream continually reader.readLine takeWhile (_ != null)

    // call/block on force in case it's not done collecting output
    def iterator = stream.force.iterator
    override def run() { stream.force }
    def slurp() = { this.start() ; this }
  }

  private val _err = new StreamedConsumer(process.getErrorStream).slurp()
  private val _out = new StreamedConsumer(process.getInputStream).slurp()

  def exitValue(): Option[Int] =
    catching(classOf[IllegalThreadStateException]) opt process.exitValue()

  def iterator = _out.iterator
  def err = _err.iterator
  override def toString() = "Process(%s)" format process.toString()
}

