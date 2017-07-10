/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc.interpreter.shell

import java.io.IOException

import InteractiveReader._

/** Reads lines from an input stream */
trait InteractiveReader {
  def interactive: Boolean

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
    if (scala.util.Properties.isMac) restartSysCalls(readOneLine(prompt), reset())
    else readOneLine(prompt)

  def initCompletion(completion: Completion): Unit = {}
}

object InteractiveReader {
  val msgEINTR = "Interrupted system call"
  def restartSysCalls[R](body: => R, reset: => Unit): R =
    try body catch {
      case e: IOException if e.getMessage == msgEINTR => reset ; body
    }

  def apply(): InteractiveReader = SimpleReader()
}

/** Collect one line of user input from the supplied reader.
  * Runs on a new thread while the REPL is initializing on the main thread.
  *
  * The user can enter text or a `:paste` command.
  *
  * TODO: obsolete the whole splash loop by making interpreter always run in separate thread from the UI,
  *       and communicating with it like we do in the presentation compiler
  */
class SplashLoop(in: InteractiveReader, prompt: String) extends Runnable {
  import java.lang.System.{lineSeparator => EOL}
  import java.util.concurrent.SynchronousQueue

  private val result = new SynchronousQueue[Option[String]]
  @volatile private var running: Boolean = _
  private var thread: Thread = _

  /** Read one line of input which can be retrieved with `line`. */
  def run(): Unit = {
    var input = ""
    try
      while (input != null && input.isEmpty && running) {
        input = in.readLine(prompt)
        if (input != null) {
          val trimmed = input.trim
          if (trimmed.length >= 3 && ":paste".startsWith(trimmed))
            input = readPastedLines
        }
      }
    finally {
      try result.put(Option(input))
      catch { case ie: InterruptedException =>  } // we may have been interrupted because the interpreter reported an error
    }
  }

  /** Process `:paste`d input. */
  private def readPastedLines: String = {
    // while collecting lines, check running flag
    var help = f"// Entering paste mode (ctrl-D to finish)%n%n"

    val text =
      try
        Iterator continually in.readLine(help) takeWhile { x =>
          help = ""
          x != null && running
        } mkString EOL trim
      catch { case ie: InterruptedException => "" } // TODO let the exception bubble up, or at least signal the interrupt happened?

    val next =
      if (text.isEmpty) "// Nothing pasted, nothing gained."
      else "// Exiting paste mode, now interpreting."
    Console println f"%n${next}%n"
    text
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

  /** Blocking. Returns Some(input) when received during splash loop, or None if interrupted (e.g., ctrl-D). */
  def line: Option[String] = result.take
}

object SplashLoop {
  def readLine(in: InteractiveReader, prompt: String)(body: => Unit): Option[String] = {
    val splash = new SplashLoop(in, prompt)
    try { splash.start; body ; splash.line }
    catch { case ie: InterruptedException => Some(null) }
    finally splash.stop()
  }
}
