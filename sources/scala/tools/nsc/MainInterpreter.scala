/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author emir
 */
// $Id$
package scala.tools.nsc;

import java.io._;
import scala.tools.nsc.util.{Position};
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter};

/** The main class for the new scala interpreter.
 */
object MainInterpreter extends Object with EvalLoop {
  // lots of stuff duplicated from Main
  val PRODUCT: String =
    System.getProperty("scala.product", "scalaint");
  val VERSION: String =
    System.getProperty("scala.version", "unknown version");
  val versionMsg = PRODUCT + " " + VERSION + " -- (c) 2002-05 LAMP/EPFL";
  val prompt = "\nnsc> ";

  private var reporter: ConsoleReporter = _;

  def error(msg: String): unit =
    reporter.error(new Position(PRODUCT),
                   msg + "\n  " + PRODUCT + " -help  gives more information");

  def errors() = reporter.errors;

  def interpret(gCompiler: Global): unit = {
    val interpreter = new Interpreter {
      val compiler: gCompiler.type = gCompiler
    };
    loop(line => try {
        interpreter.interpret(line.trim(), reporter)
      } catch {
        case e: Exception => {
          reporter.info(null,e.getMessage(),true);
          //e.printStackTrace();
        }
      }
    )
  }

  def process(args: Array[String]): unit = {
    reporter = new ConsoleReporter();
    val command = new CompilerCommand(List.fromArray(args), error, false);
    reporter.prompt = (command.settings.prompt.value);
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else if (command.settings.help.value) // 2do replace with InterpCommand
      reporter.info(null, command.usageMsg, true)

    else {
      try {
        val compiler = new Global(command.settings, reporter);
        interpret(compiler);
      } catch {
  case ex @ FatalError(msg) =>
    if (command.settings.debug.value)
      ex.printStackTrace();
    reporter.error(null, "fatal error: " + msg);
      }
      reporter.printSummary()
    }
  }

  def main(args: Array[String]): unit = {
    process(args);
    System.exit(if (reporter.errors > 0) 1 else 0);
  }

}
