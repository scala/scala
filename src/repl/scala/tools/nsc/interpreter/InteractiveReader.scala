/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.IOException
import session.History
import InteractiveReader._
import Properties.isMac

/** Reads lines from an input stream */
trait InteractiveReader {
  def postInit(): Unit = {}

  val interactive: Boolean

  def reset(): Unit
  def history: History
  def completion: Completion
  def redrawLine(): Unit

  def readYesOrNo(prompt: String, alt: => Boolean): Boolean = readOneKey(prompt) match {
    case 'y'  => true
    case 'n'  => false
    case -1   => false // EOF
    case _    => alt
  }

  protected def readOneLine(prompt: String): String
  protected def readOneKey(prompt: String): Int

  def readLine(prompt: String): String =
    // hack necessary for OSX jvm suspension because read calls are not restarted after SIGTSTP
    if (isMac) restartSysCalls(readOneLine(prompt), reset())
    else readOneLine(prompt)
}

object InteractiveReader {
  val msgEINTR = "Interrupted system call"
  def restartSysCalls[R](body: => R, reset: => Unit): R =
    try body catch {
      case e: IOException if e.getMessage == msgEINTR => reset ; body
    }

  def apply(): InteractiveReader = SimpleReader()
  @deprecated("Use `apply` instead.", "2.9.0")
  def createDefault(): InteractiveReader = apply() // used by sbt
}

/** Collect one line of user input from the supplied reader.
 *  Runs on a new thread while the REPL is initializing on the main thread.
 *
 *  The user can enter text or a `:paste` command.
 */
class SplashLoop(reader: InteractiveReader, prompt: String) extends Runnable {
  import java.util.concurrent.SynchronousQueue
  import scala.compat.Platform.EOL

  private val result = new SynchronousQueue[Option[String]]
  @volatile private var running: Boolean = _
  private var thread: Thread = _

  /** Read one line of input which can be retrieved with `line`. */
  def run(): Unit = {
    var line = ""
    try
      do {
        line = reader.readLine(prompt)
        if (line != null) {
          line = process(line.trim)
        }
      } while (line != null && line.isEmpty && running)
    finally {
      result.put(Option(line))
    }
  }

  /** Check for `:paste` command. */
  private def process(line: String): String = {
    def isPrefix(s: String, p: String, n: Int) = (
      //s != null && p.inits.takeWhile(_.length >= n).exists(s == _)
      s != null && s.length >= n && s.length <= p.length && s == p.take(s.length)
    )
    if (isPrefix(line, ":paste", 3)) {
      // while collecting lines, check running flag
      var help = f"// Entering paste mode (ctrl-D to finish)%n%n"
      def readWhile(cond: String => Boolean) = {
        Iterator continually reader.readLine(help) takeWhile { x =>
          help = ""
          x != null && cond(x)
        }
      }
      val text = (readWhile(_ => running) mkString EOL).trim
      val next =
        if (text.isEmpty) "// Nothing pasted, nothing gained."
        else "// Exiting paste mode, now interpreting."
      Console println f"%n${next}%n"
      text
    } else {
      line
    }
  }

  def start(): Unit = result.synchronized {
    require(thread == null, "Already started")
    thread = new Thread(this)
    running = true
    thread.start()
  }

  def stop(): Unit = result.synchronized {
    running = false
    if (thread != null) thread.interrupt()
    thread = null
  }

  /** Block for the result line, or null on ctl-D. */
  def line: String = result.take getOrElse null
}
object SplashLoop {
  def apply(reader: SplashReader, prompt: String): SplashLoop = new SplashLoop(reader, prompt)
}

/** Reader during splash. Handles splash-completion with a stub, otherwise delegates. */
class SplashReader(reader: InteractiveReader, postIniter: InteractiveReader => Unit) extends InteractiveReader {
  /** Invoke the postInit action with the underlying reader. */
  override def postInit(): Unit = postIniter(reader)

  override val interactive: Boolean = reader.interactive

  override def reset(): Unit = reader.reset()
  override def history: History = reader.history
  override val completion: Completion = NoCompletion
  override def redrawLine(): Unit = reader.redrawLine()

  override protected[interpreter] def readOneLine(prompt: String): String = ???   // unused
  override protected[interpreter] def readOneKey(prompt: String): Int     = ???   // unused

  override def readLine(prompt: String): String = reader.readLine(prompt)
}
object SplashReader {
  def apply(reader: InteractiveReader)(postIniter: InteractiveReader => Unit) =
    new SplashReader(reader, postIniter)
}
