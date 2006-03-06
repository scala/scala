/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author emir
 */
// $Id$
package scala.tools.nsc

import java.io.{BufferedReader, FileReader, IOException, InputStreamReader}
import scala.tools.nsc.util.{Position}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}

/** The main class for the new scala interpreter.
 */
object MainInterpreter {
  val reporter = new ConsoleReporter()

  var interpreter: Interpreter = _

  /** print a friendly help message */
  def printHelp = {
    Console.println("This is an interpreter for Scala.")
    Console.println("Type in expressions to have them evaluated.")
    Console.println("Type :quit to exit the interpreter.")
    Console.println("Type :compile followed by a filename to compile a complete Scala file.")
    Console.println("Type :load followed by a filename to load a sequence of interpreter commands.")
    Console.println("Type :help to repeat this message later.")
  }

  /** A simple, generic read-eval-print loop with a pluggable eval-print function.
      Loop reading lines of input and invoking the eval-print function.
      Stop looping when eval-print returns false.  */
  def repl(evpr: String => Boolean): Unit = {
    val in = new BufferedReader(new InputStreamReader(System.in))

    while(true) {
      Console.print("\nscala> ")
      var line = in.readLine()
      if(line == null)
        return ()  // assumes null means EOF

      val keepGoing = evpr(line)
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
        Console.println("Error opening file: " + filename)
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
        Console.println("That command requires a filename to be specified.")
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
        case _ => Console.println("Unknown command.  Type :help for help.")
      }
    else
      interpretOne(line)
    true
  }

  /** the main interpreter loop */
  def interpretLoop(compiler: Global): unit = {
    interpreter = new Interpreter(compiler, str => Console.print(str))
    repl(command)
    interpreter.close
  }

  /** process the command-line arguments and do as they request */
  def process(args: Array[String]): Unit = {
    val command = new CompilerCommand(List.fromArray(args), error, false)
    reporter.prompt = command.settings.prompt.value
    if (command.settings.help.value) {
      reporter.info(null, command.usageMsg, true)
    }
    else {
      printHelp
      interpretLoop(new Global(command.settings, reporter))
    }
  }

  def main(args: Array[String]): Unit = {
    process(args)
  }

}
