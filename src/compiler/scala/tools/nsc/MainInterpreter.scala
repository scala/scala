/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author emir
 */
// $Id$
package scala.tools.nsc

import java.io.{BufferedReader, FileReader, IOException, InputStreamReader, PrintWriter}
import scala.tools.nsc.util.{Position}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}

/** The main loop of the Scala interpreter.  After instantiation, clients
  * should call the main() method
  */
class InterpreterLoop(in: BufferedReader, out: PrintWriter) {
  def this() = this(new BufferedReader(new InputStreamReader(System.in)),
                    new PrintWriter(System.out))

  val reporter = new ConsoleReporter(in, out)

  var interpreter: Interpreter = _

  /** print a friendly help message */
  def printHelp = {
    out.println("This is an interpreter for Scala.")
    out.println("Type in expressions to have them evaluated.")
    out.println("Type :quit to exit the interpreter.")
    out.println("Type :compile followed by a filename to compile a complete Scala file.")
    out.println("Type :load followed by a filename to load a sequence of interpreter commands.")
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
      if(line == null)
        return ()  // assumes null means EOF

      val keepGoing = command(line)
      if(!keepGoing)
        return ()  // the evpr function said to stop
    }
  }

  /** interpret one line of code submitted by the user */
  def interpretOne(line: String): Unit = {
    try {
      interpreter.interpret(line)
    } catch {
      case e: Exception =>
        reporter.info(null, "Exception occurred: " + e.getMessage(), true)
        //e.printStackTrace()
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
    while(true) {
      val line = in.readLine
      if (line == null) {
        fileIn.close
        return ()
      }
      command(line)
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
      action(filename)
    }

    if (line.startsWith(":"))
      line match {
        case ":help" => printHelp
        case ":quit" => return false
        case _ if line.startsWith(":compile") => withFile(line)(f => interpreter.compile(f))
        case _ if line.startsWith(":load") => withFile(line)(f => interpretAllFrom(f))
        case _ => out.println("Unknown command.  Type :help for help.")
      }
    else if(line.startsWith("#!/")) // skip the first line of Unix scripts
    	()
    else if(line.startsWith("exec scalaint ")) // skip the second line of Unix scripts
      ()
    else
      interpretOne(line)
    true
  }
	def parentClassLoader0 : ClassLoader = null;

	/** process command-line arguments and do as they request */
  def main(args: Array[String]): unit = {
    val command = new InterpreterCommand(List.fromArray(args), error)

    reporter.prompt = command.settings.prompt.value
    if (command.settings.help.value) {
      reporter.info(null, command.usageMsg, true)
      return ()
    }

    val compiler = new Global(command.settings, reporter)
    interpreter = new Interpreter(compiler, out.print) {
      override protected def parentClassLoader = parentClassLoader0;
    }

    try {
      if(!command.files.isEmpty) {
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
      interpreter.close
    }
  }

}


/** A wrapper that provides a command-line interface */
object MainInterpreter {
 	def main(args: Array[String]): Unit = {
     (new InterpreterLoop).main(args)
 	}
}
