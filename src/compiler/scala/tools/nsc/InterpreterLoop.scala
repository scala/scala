/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Alexander Spoon
 */
// $Id$

package scala.tools.nsc

import java.io._
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.{Position}

/** The main loop of the command-line interface to the Scala interpreter.
  * After instantiation, clients should call the main() method
  */
class InterpreterLoop(in: BufferedReader, out: PrintWriter) {
  def this() = this(new BufferedReader(new InputStreamReader(System.in)),
                    new PrintWriter(System.out))

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
  def closeInterpreter = {
    if(interpreter != null) {
      interpreter.close
      interpreter = null
    }
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
  def printHelp = {
    out.println("This is an interpreter for Scala.")
    out.println("Type in expressions to have them evaluated.")
    out.println("Type :quit to exit the interpreter.")
    out.println("Type :compile followed by a filename to compile a complete Scala file.")
    out.println("Type :load followed by a filename to load a sequence of interpreter commands.")
    out.println("Type :replay to reset execution and replay all previous commands.")
    out.println("Type :help to repeat this message later.")
  }

  /** The main read-eval-print loop for the interpereter.  It calls
      command() for each line of input, and stops when command()
      returns false */
  def repl(): Unit = {
    while(true) {
      out.print("\nscala> ")
      out.flush
      var line = in.readLine()
      if (line == null)
        return ()  // assumes null means EOF

      val keepGoing = command(line)
      if (!keepGoing)
        return ()  // the evpr function said to stop
    }
  }

  /** interpret all lines from a specified file */
  def interpretAllFrom(filename: String): Unit = {
    val fileIn = try {
      new FileReader(filename)
    } catch {
      case _:IOException =>
        out.println("Error opening file: " + filename)
        null
    }
    if (fileIn == null) return ()
    val in = new BufferedReader(fileIn)
    while (true) {
      val line = in.readLine
      if (line == null) {
        fileIn.close
        return ()
      }
      command(line)
    }
  }

  /** create a new interpreter and replay all commands so far */
  def replay = {
    closeInterpreter
    createInterpreter
    for(val cmd <- replayCommands) {
      out.println("Replaying: " + cmd)
      command(cmd)
      out.println
    }
  }

  /** run one command submitted by the user */
  def command(line: String): Boolean = {
    def withFile(command: String)(action: String => Unit): Unit = {
      val spaceIdx = command.indexOf(' ')
      if (spaceIdx <= 0) {
        out.println("That command requires a filename to be specified.")
        return ()
      }
      val filename = command.substring(spaceIdx).trim
      if(! new File(filename).exists) {
        out.println("That file does not exist")
        return ()
      }

      action(filename)
    }

    val helpRegexp    = ":h(e(l(p)?)?)?"
    val quitRegexp    = ":q(u(i(t)?)?)?"
    val compileRegexp = ":c(o(m(p(i(l(e)?)?)?)?)?)?.*"
    val loadRegexp    = ":l(o(a(d)?)?)?.*"
    val replayRegexp  = ":r(e(p(l(a(y)?)?)?)?)?.*"

    if (line.matches(helpRegexp))
      printHelp
    else if (line.matches(quitRegexp))
      return false
    else if (line.matches(compileRegexp)) {
      withFile(line)(f => {
        interpreter.compile(f)
        addReplay(line)
      })
    }
    else if (line.matches(loadRegexp)) {
      withFile(line)(f => {
        interpretAllFrom(f)
        addReplay(line)
      })
    }
    else if (line.matches(replayRegexp))
      replay
    else if (line.startsWith(":"))
      out.println("Unknown command.  Type :help for help.")
    else if (line.startsWith("#!/")) // skip the first line of Unix scripts
      ()
    else {
      if(interpreter.interpret(line))
        addReplay(line)
    }
    true
  }


  /** process command-line arguments and do as they request */
  def main(args: Array[String]): unit = {
    def error1(msg: String): Unit = out.println("scalaint: " + msg)
    val command = new InterpreterCommand(List.fromArray(args), error1)
    settings = command.settings

    uglinessxxx =
      new java.net.URLClassLoader(
                 settings.classpath.value.split(File.pathSeparator).
                         map(s => new File(s).toURL),
                 ClassLoader.getSystemClassLoader)



    if (!command.ok || command.settings.help.value) {
      // either the command line is wrong, or the user
      // explicitly requested a help listing
      out.println(command.usageMsg)
      out.flush
      return ()
    }

    createInterpreter

    try {
      if (!command.files.isEmpty) {
        interpreter.beQuiet
        command.files match {
          case List(filename) => interpretAllFrom(filename)
          case _ => out.println(
              "Sorry, arguments to interpreter scripts are not currently supported.")
        }
      } else {
        printHelp
        repl
      }
    } finally {
      closeInterpreter
    }
  }

}
