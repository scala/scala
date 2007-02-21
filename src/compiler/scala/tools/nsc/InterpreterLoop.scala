/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Alexander Spoon
 */
// $Id$

package scala.tools.nsc

import java.lang.System
import java.lang.ClassLoader
import java.io.{BufferedReader, InputStreamReader, File, FileReader, PrintWriter}
import java.io.IOException

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.{Position}
import nsc.{InterpreterResults=>IR}

/** The main loop of the command-line interface to the
 *  <a href="http://scala-lang.org/" target="_top">Scala</a> interpreter.
 *  After instantiation, clients should call the <code>main()</code> method.
 *
 *  @author  Lex Spoon
 *  @version 1.0
 */
class InterpreterLoop(in0: BufferedReader, out: PrintWriter) {
  def this() = this(new BufferedReader(new InputStreamReader(System.in)),
                    new PrintWriter(System.out))

  /** The input stream from which interpreter commands come */
  var in = in0

  /** Whether the interpreter is interactive (like normal), or instead
    * is reading from a file.
    */
  var interactive = true

  var settings: Settings = _ // set by main()
  var interpreter: Interpreter = null // set by createInterpreter()

  /** A reverse list of commands to replay if the user
    * requests a :replay */
  var replayCommandsRev: List[String] = Nil

  /** A list of commands to replay if the user requests a :replay */
  def replayCommands = replayCommandsRev.reverse

  /** Record a command for replay should the user requset a :replay */
  def addReplay(cmd: String) =
    replayCommandsRev = cmd :: replayCommandsRev

  /** Close the interpreter, if there is one, and set
    * interpreter to null. */
  def closeInterpreter =
    if (interpreter ne null) {
      interpreter.close
      interpreter = null
    }

  /* As soon as the Eclipse plugin no longer needs it, delete uglinessxxx,
   * parentClassLoader0, and the parentClassLoader method in Interpreter
   */
  var uglinessxxx: ClassLoader = _
  def parentClassLoader0: ClassLoader = uglinessxxx

  /** Create a new interpreter.  Close the old one, if there
    * is one. */
  def createInterpreter = {
    closeInterpreter

    interpreter = new Interpreter(settings, out) {
      override protected def parentClassLoader = parentClassLoader0;
    }
  }

  /** print a friendly help message */
  def printHelp {
    out.println("This is an interpreter for Scala.")
    out.println("Type in expressions to have them evaluated.")
    out.println("Type :help to repeat this message.")
    out.println("Type :load followed by a filename to load a Scala file.")
    out.println("Type :replay to reset execution and replay all previous commands.")
    out.println("Type :quit to exit the interpreter.")
  }

  /** Print a welcome message */
  def printWelcome {
    out.println("This is an interpreter for Scala.")
    out.println("Type in expressions to have them evaluated.")
    out.println("Type :help for more information.")
  }

  /** The main read-eval-print loop for the interpereter.  It calls
   *  <code>command()</code> for each line of input, and stops when
   *  <code>command()</code> returns <code>false</code>.
   */
  def repl {
    var firstTime = true
    while (true) {
      if (interactive) {
        out.print("\nscala> ")
        out.flush
        if (firstTime) {
          interpreter.prime
          firstTime = false
        }
      }
      var line = in.readLine()
      if (line eq null)
        return ()  // assumes null means EOF

      val (keepGoing, finalLineMaybe) = command(line)

      if (!keepGoing)
        return

      finalLineMaybe match {
        case Some(finalLine) => addReplay(finalLine)
        case None => ()
      }
    }
  }

  /** interpret all lines from a specified file */
  def interpretAllFrom(filename: String) {
    val fileIn = try {
      new FileReader(filename)
    } catch {
      case _:IOException =>
        out.println("Error opening file: " + filename)
        return
    }
    val oldIn = in
    val oldInteractive = interactive
    try {
      in = new BufferedReader(fileIn)
      interactive = false
      out.println("Loading " + filename + "...")
      out.flush
      repl
    } finally {
      in = oldIn
      interactive = oldInteractive
      fileIn.close
    }
  }

  /** create a new interpreter and replay all commands so far */
  def replay {
    closeInterpreter
    createInterpreter
    for (val cmd <- replayCommands) {
      out.println("Replaying: " + cmd)
      command(cmd)
      out.println
    }
  }

  /** Run one command submitted by the user.  Three values are returned:
    * (1) whether to keep running, (2) the line to record for replay,
    * if any. */
  def command(line: String): (Boolean, Option[String]) = {
    def withFile(command: String)(action: String => Unit): Unit = {
      val spaceIdx = command.indexOf(' ')
      if (spaceIdx <= 0) {
        out.println("That command requires a filename to be specified.")
        return ()
      }
      val filename = command.substring(spaceIdx).trim
      if (! new File(filename).exists) {
        out.println("That file does not exist")
        return ()
      }
      action(filename)
    }

    val helpRegexp    = ":h(e(l(p)?)?)?"
    val quitRegexp    = ":q(u(i(t)?)?)?"
    val loadRegexp    = ":l(o(a(d)?)?)?.*"
    val replayRegexp  = ":r(e(p(l(a(y)?)?)?)?)?.*"

    var shouldReplay: Option[String] = None

    if (line.matches(helpRegexp))
      printHelp
    else if (line.matches(quitRegexp))
      return (false, None)
    else if (line.matches(loadRegexp)) {
      withFile(line)(f => {
        interpretAllFrom(f)
        shouldReplay = Some(line)
      })
    }
    else if (line.matches(replayRegexp))
      replay
    else if (line.startsWith(":"))
      out.println("Unknown command.  Type :help for help.")
    else
      shouldReplay = interpretStartingWith(line)

    (true, shouldReplay)
  }

  /** Interpret expressions starting with the first line.
    * Read lines until a complete compilation unit is available
    * or until a syntax error has been seen.  If a full unit is
    * read, go ahead and interpret it.  Return the full string
    * to be recorded for replay, if any.
    */
  def interpretStartingWith(code: String): Option[String] =
  {
    interpreter.interpret(code) match {
      case IR.Success => Some(code)
      case IR.Error => None
      case IR.Incomplete =>
        if (interactive && code.endsWith("\n\n")) {
          out.println("Two blank lines seen.  Aborting this expression.")
          None
        } else {
          if (interactive) {
            out.print("     | ")
            out.flush
          }
          val nextLine = in.readLine
          if (nextLine == null)
            None  // end of file
          else
            interpretStartingWith(code + "\n" + nextLine)
        }
    }
  }


  def main(settings: Settings) = {
    this.settings = settings

    uglinessxxx =
      new java.net.URLClassLoader(
                 settings.classpath.value.split(File.pathSeparator).
                         map(s => new File(s).toURL),
                 ClassLoader.getSystemClassLoader)

    createInterpreter

    try {
      printWelcome
      repl
    } finally {
      closeInterpreter
    }
  }

  /** process command-line arguments and do as they request */
  def main(args: Array[String]) {
    def error1(msg: String): Unit = out.println("scala: " + msg)
    val command = new InterpreterCommand(List.fromArray(args), error1)

    if (!command.ok || command.settings.help.value) {
      // either the command line is wrong, or the user
      // explicitly requested a help listing
      out.println(command.usageMsg)
      out.flush
    }
    else
      main(command.settings)
  }
}
