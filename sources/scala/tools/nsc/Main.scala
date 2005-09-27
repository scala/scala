/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc;

import java.io._;
import scala.tools.util.{Position, Reporter, ConsoleReporter}

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main {

  val PRODUCT: String =
    System.getProperty("scala.product", "scalac");
  val VERSION: String =
    System.getProperty("scala.version", "unknown version");
  val versionMsg = PRODUCT + " " + VERSION + " -- (c) 2002-05 LAMP/EPFL";
  val prompt = "\nnsc> ";

  private var reporter: ConsoleReporter = _;

  def error(msg: String): unit =
    reporter.error(new Position(PRODUCT),
      msg + "\n  " + PRODUCT + " -help  gives more information");

  def errors() = reporter.errors();

  def interactive(gCompiler: Global): unit = {
    val in = new BufferedReader(new InputStreamReader(System.in));
    val interpreter = new Interpreter {
      val compiler: gCompiler.type = gCompiler
    };
    System.out.print(prompt);
    var line = in.readLine();
    while (line != null && line.length() > 0) {
      interpreter.interpret(line.trim(), reporter);
      System.out.print(prompt);
      line = in.readLine();
    }
  }

  def process(args: Array[String]): unit = {
    reporter = new ConsoleReporter();
    val command = new CompilerCommand(List.fromArray(args), error, false);
    reporter.prompt(command.settings.prompt.value);
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else if (command.settings.help.value)
      reporter.info(null, command.usageMsg, true)
    else {
      try {
        val compiler = new Global(command.settings, reporter);
        if (command.settings.interactive.value)
          interactive(compiler);
        else if (command.files.isEmpty)
          reporter.info(null, command.usageMsg, true)
        else
          compiler.compile(command.files);
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
    System.exit(if (reporter.errors() > 0) 1 else 0);
  }
}
