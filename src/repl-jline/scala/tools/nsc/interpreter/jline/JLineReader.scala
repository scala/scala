/** NSC -- new Scala compiler
  *
  * Copyright 2005-2015 LAMP/EPFL
  *
  * @author Stepan Koltsov
  * @author Adriaan Moors
  */

package scala.tools.nsc.interpreter.jline

import java.util.{Collection => JCollection, List => JList}

import _root_.jline.{console => jconsole}
import jline.console.ConsoleReader
import jline.console.completer.{CandidateListCompletionHandler, CompletionHandler, Completer, ArgumentCompleter}
import jconsole.history.{History => JHistory}

import scala.tools.nsc.interpreter
import scala.tools.nsc.interpreter.{Completion, JLineCompletion, NoCompletion}
import scala.tools.nsc.interpreter.Completion.Candidates
import scala.tools.nsc.interpreter.session.History

/**
 * Reads from the console using JLine.
 *
 * Eagerly instantiates all relevant JLine classes, so that we can detect linkage errors on `new JLineReader` and retry.
 */
class InteractiveReader(completer: () => Completion) extends interpreter.InteractiveReader {
  val interactive = true

  val history: History = new JLineHistory.JLineFileHistory()

  private val consoleReader = {
    val reader = new JLineConsoleReader()

    reader setPaginationEnabled interpreter.isPaged

    // turn off magic !
    reader setExpandEvents false

    // enable detecting pasted tab char (when next char is immediately available) which is taken raw, not completion
    reader setCopyPasteDetection true

    reader setHistory history.asInstanceOf[JHistory]

    reader
  }

  private[this] var _completion: Completion = interpreter.NoCompletion
  def completion: Completion = _completion

  override def postInit() = {
    _completion = completer()

    consoleReader.initCompletion(completion)
  }

  def reset()                     = consoleReader.getTerminal().reset()
  def redrawLine()                = withReader(_.redrawLineAndFlush())
  def readOneLine(prompt: String) = withReader(_.readLine(prompt))
  def readOneKey(prompt: String)  = withReader(_.readOneKey(prompt))
  private def withReader[T](f: JLineConsoleReader => T): T = {
    // SI-8308 Get JLine working after suspend/resume
    //
    // Borrowed this logic from SBT.
    // My testing on MacOS X suggests that only the call to `resume` in the
    // signal handler for SIGCONT is needed to make things work. The line is redrawn
    // and tab completion works immediately.
    //
    // However, the `init` / `restore` pair will get JLine back up and running on
    // systems that don't support signal handlers (sun.misc.SignalHandler isn't portable)
    // so it seems to serve as a useful backstop. I'm not sure whether or not it serves
    // some other purpose.
    consoleReader.getTerminal.init()
    try {
      Signalling().withSigContHandler(() => resume()){
        f(consoleReader)
      }
    } finally {
      consoleReader.getTerminal.restore()
    }
  }

  private def resume() {
    jline.TerminalFactory.reset()
    redrawLine()
    consoleReader.getTerminal.init()
  }
}

// implements a jline interface
private class JLineConsoleReader extends jconsole.ConsoleReader with interpreter.VariColumnTabulator {
  val isAcross   = interpreter.isAcross
  val marginSize = 3

  def width  = getTerminal.getWidth()
  def height = getTerminal.getHeight()

  private def morePrompt = "--More--"

  private def emulateMore(): Int = {
    val key = readOneKey(morePrompt)
    try key match {
      case '\r' | '\n' => 1
      case 'q' => -1
      case _ => height - 1
    }
    finally {
      eraseLine()
      // TODO: still not quite managing to erase --More-- and get
      // back to a scala prompt without another keypress.
      if (key == 'q') {
        putString(getPrompt())
        redrawLine()
        flush()
      }
    }
  }

  override def printColumns(items: JCollection[_ <: CharSequence]): Unit = {
    import scala.tools.nsc.interpreter.javaCharSeqCollectionToScala
    printColumns_(items: List[String])
  }

  private def printColumns_(items: List[String]): Unit = if (items exists (_ != "")) {
    val grouped = tabulate(items)
    var linesLeft = if (isPaginationEnabled()) height - 1 else Int.MaxValue
    grouped foreach { xs =>
      println(xs.mkString)
      linesLeft -= 1
      if (linesLeft <= 0) {
        linesLeft = emulateMore()
        if (linesLeft < 0)
          return
      }
    }
  }

  def readOneKey(prompt: String) = {
    this.print(prompt)
    this.flush()
    this.readCharacter()
  }

  def eraseLine() = resetPromptLine("", "", 0)

  def redrawLineAndFlush(): Unit = {
    flush(); drawLine(); flush()
  }

  // A hook for running code after the repl is done initializing.
  def initCompletion(completion: Completion): Unit = {
    this setBellEnabled false

    // adapt the JLine completion interface
    def completer =
      new Completer {
        val tc = completion.completer()
        def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
          val buf = if (_buf == null) "" else _buf
          val Candidates(newCursor, newCandidates) = tc.complete(buf, cursor)
          newCandidates foreach (candidates add _)
          newCursor
        }
      }
    getCompletionHandler match {
      case clch: CandidateListCompletionHandler => clch.setPrintSpaceAfterFullCompletion(false)
    }

    // a last bit of nastiness: parsing help depending on the flavor of completer (fixme)
    completion match {
      case _: JLineCompletion =>
        val jlineCompleter = new ArgumentCompleter(new JLineDelimiter, completer)
        jlineCompleter setStrict false
        this addCompleter jlineCompleter
      case NoCompletion       => ()
      case _                  => this addCompleter completer
    }

    setAutoprintThreshold(400) // max completion candidates without warning
  }
}

trait Signalling {
  def withSigContHandler[T](handler: () => Unit)(action: => T): T
}
object Signalling {
  def apply(): Signalling = instance

  private lazy val instance: Signalling =
    if (DisableSigCont) NoSignalling
    else try { new SunMiscSignalViaReflection } catch { case _: Throwable => NoSignalling}

  // Disable unconditionally on J9 as it prints a stacktrace to console (e.g. https://github.com/sbt/sbt/issues/1027)
  private val DisableSigCont = sys.props.contains("scala.repl.disable.cont") || (sys.props("java.vm.name") == "IBM J9 VM")
  private object NoSignalling extends Signalling {
    override def withSigContHandler[T](handler: () => Unit)(action: => T): T = action
  }
  private class SunMiscSignalViaReflection extends Signalling {
    import java.lang.reflect.InvocationTargetException
    import java.lang.reflect.{Method, Proxy, InvocationHandler, Constructor}

    def withSigContHandler[T](handler: () => Unit)(action: => T): T = {
      val newHandler = newSignalHandler(handler)
      val oldHandler = Signal_handle(sigCont, newHandler)
      try action finally Signal_handle(sigCont, oldHandler)
    }

    @inline private def unwrapITE[T](f: => T): T =
      try { f } catch { case ite: InvocationTargetException => throw ite.getTargetException }
    val sun_misc_Signal: Class[_] = Class.forName("sun.misc.Signal")
    val sun_misc_SignalHandler: Class[_] = Class.forName("sun.misc.SignalHandler")
    val sun_misc_Signal_handle: Method = sun_misc_Signal.getDeclaredMethod("handle", sun_misc_Signal, sun_misc_SignalHandler)
    val sun_misc_Signal_init: Constructor[_] = sun_misc_Signal.getConstructor(classOf[String])
    val sigCont = newSignal("CONT") // create the signal here (within a try/catch) to avoid crashing on windows.

    def newSignal(s: String): AnyRef = unwrapITE(sun_misc_Signal_init.newInstance(s).asInstanceOf[AnyRef])
    def newSignalHandler(s: () => Unit): AnyRef = {
      val ih = new InvocationHandler {
        override def invoke(proxy: scala.Any, method: Method, args: Array[AnyRef]): AnyRef = {s(); null}
      }
      Proxy.newProxyInstance(getClass.getClassLoader, Array(sun_misc_SignalHandler), ih)
    }
    def Signal_handle(signal: AnyRef, handler: AnyRef): AnyRef = unwrapITE {
      sun_misc_Signal_handle.invoke(null, signal, handler)
    }
  }
}
